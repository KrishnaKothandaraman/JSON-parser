module Main where
import Parsing
import Control.Applicative hiding(many)

--TODO: add support for unicode
--TODO: create shell command to run file
data JsonValue = JsonNull
                 | JsonBool Bool 
                 | JsonInteger Integer 
                 | JsonFloat Float 
                 | JsonString String
                 | JsonArray [JsonValue]
                 | JsonObject [(String, JsonValue)]
                 deriving (Eq, Show)

jsonString :: Parser JsonValue
jsonString = do token $ char '"'
                x <- many (sat (/='"'))
                token $ char '"'
                return (JsonString x)

jsonFloat :: Parser Float 
jsonFloat =  do x <- many1 digit
                char '.'
                y <- many1 digit
                return ((read :: String -> Float) (x++"."++y))

jsonInteger :: Parser Integer
jsonInteger = do x <- many1 digit
                 return ((read :: String -> Integer) x)

jsonNumber :: Parser JsonValue
jsonNumber = (do space
                 sign <- char '-'
                 x <- jsonFloat
                 return (JsonFloat $ (-x))
                 ) 
                +++ 
             (do space
                 x <- jsonFloat
                 return (JsonFloat $ x)
                ) 
                +++ 
             (do space
                 sign <- char '-'
                 x <- jsonInteger 
                 return (JsonInteger $ (-x)))
                +++ 
             (do space
                 x <- jsonInteger 
                 return (JsonInteger $ x))

jsonBool :: Parser JsonValue
jsonBool = (do x <- token $ string "true"
               return (JsonBool True)) +++
           (do x <- token $ string "false"
               return (JsonBool False)
           )

jsonNull :: Parser JsonValue
jsonNull = do x <- token $ string "null"
              return (JsonNull)

jsonArrayElement :: Parser JsonValue
jsonArrayElement = do token $ char ','
                      x <- token $ jsonValue
                      return x

jsonArray :: Parser JsonValue
jsonArray = do token $ char '['
               x <- token $ many jsonValue
               y <- token $ many jsonArrayElement
               token $ char ']'
               return (JsonArray (x++y))

pairs :: Parser (String, JsonValue)
pairs = do char '"' 
           key <- token $ many1 $ sat (/='"')
           token (char '"')
           token (char ':')
           value <- token $ jsonValue
           token $ many (char ',')
           return (key, value)

jsonObject :: Parser JsonValue
jsonObject = do token $ char '{'
                x <- token $ many pairs
                token $ char '}'
                return (JsonObject x) 

jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject

parseJson :: Show a => FilePath -> Parser a -> IO a
parseJson fileName parser = do inp <- readFile fileName
                               return (fst $ head $ parse parser inp)

printJsonNull :: Int -> String
printJsonNull tabs = (replicate tabs '\t') ++ "null"

printJsonBool :: Bool -> Int -> String
printJsonBool (True) tabs = (replicate tabs '\t') ++ "true"
printJsonBool (False) tabs = (replicate tabs '\t') ++ "false"

printJsonInteger :: Integer -> Int -> String
printJsonInteger x tabs = (replicate tabs '\t') ++ show x

printJsonFloat :: Float -> Int -> String
printJsonFloat f tabs = (replicate tabs '\t') ++ show f

printJsonArray :: [JsonValue] -> Int -> String
printJsonArray [] _ = []
printJsonArray [x] tabs = (replicate tabs '\t') ++ printJsonVal x tabs ++ "\n"
printJsonArray (x:xs) tabs  = (replicate tabs '\t') ++ (printJsonVal x tabs) ++ ",\n" ++ (printJsonArray xs tabs)

printJsonObject :: [(String, JsonValue)] -> Int -> String
printJsonObject [] _ = "\n"
printJsonObject [x] tabs = (replicate tabs '\t') ++ ("\"" ++ (fst x) ++ "\"") ++ " : " ++ (printJsonVal (snd x) 0) ++ "\n"
printJsonObject (x:xs) tabs = (replicate tabs '\t') ++ ("\"" ++ (fst x) ++ "\"") ++ " : " ++ (printJsonVal (snd x) 0) ++ ",\n" ++ (printJsonObject xs tabs)

printJsonVal :: JsonValue -> Int -> String
printJsonVal (JsonNull) tabs = printJsonNull tabs   
printJsonVal (JsonBool t) tabs = printJsonBool t tabs   
printJsonVal (JsonInteger x) tabs = printJsonInteger x tabs  
printJsonVal (JsonFloat f) tabs = printJsonFloat f tabs   
printJsonVal (JsonString x) tabs = (replicate tabs '\t') ++ "\"" ++ x ++ "\""
printJsonVal (JsonArray x) tabs = "[\n" ++ (printJsonArray x (tabs+1)) ++ (replicate (tabs+1) '\t') ++ "]"  
printJsonVal (JsonObject x) tabs = (replicate tabs '\t') ++ "{\n" ++ (printJsonObject x (tabs+1)) ++ (replicate tabs '\t') ++ "}" 

dumpParsedJson :: FilePath -> FilePath -> Parser JsonValue -> IO ()
dumpParsedJson inputFile outputFile parser = do inp <- readFile inputFile
                                                writeFile outputFile (printJsonVal (fst $ head$ parse parser inp) 0)

main :: IO ()
main = undefined
