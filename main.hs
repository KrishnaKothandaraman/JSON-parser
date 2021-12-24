module Main where
import Parsing
import Control.Applicative hiding(many)

--TODO: add support for spaces
--TODO: add support for escaped characters
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
jsonFloat =  do space
                x <- many1 digit
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

parseJson :: FilePath -> Parser a -> IO ([(a,String)])
parseJson fileName parser = do inp <- readFile fileName
                               return (parse parser inp)


main :: IO ()
main = do x <- readFile "json.txt"
          putStrLn x