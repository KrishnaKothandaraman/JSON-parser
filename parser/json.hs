module Json where
import Parsing
import System.FilePath.Posix
import Data.Char hiding(isControl)
import Numeric
import Control.Applicative hiding(many)
import Text.Read(readMaybe)
import System.Directory

--TODO: add support for escaped characters
--TODO: add support for unicode
--TODO: create shell command to run file
data JsonValue = JsonNull
                 | JsonBool Bool 
                 | JsonNumber Double 
                 | JsonString String
                 | JsonArray [JsonValue]
                 | JsonObject [(String, JsonValue)]
                 deriving (Eq, Show)

jsonString :: Parser JsonValue
jsonString = do token $ char '"'
                x <- stringLiteral
                token $ char '"'
                return (JsonString x)

-- author      : https://github.com/tsoding/haskell-json/blob/master/Main.hs
-- modified by : Krishna Kothandaraman
{-
See page 12 of
http://www.ecma-international.org/publications/files/ECMA-ST/ECMA-404.pdf
-}
-- | Parser for doubles
doubleLiteral :: Parser (Maybe Double)
doubleLiteral =
  doubleFromParts
    <$> (minus <|> pure 1)
    <*> (readMaybe <$> validDigits)
    <*> ((readMaybe <$> (('0':) <$> ((:) <$> char '.' <*> digits))) <|> pure (Just 0))
    <*> ((e *> ((*) <$> (plus <|> minus <|> pure 1) <*> (read <$> digits))) <|> pure 0)
  where
    validDigits = (do x <- nonZeroDigit
                      xs <- many digit
                      return (x:xs)) <|> (string "0") <|> failure
    digits = many digit
    minus = (-1) <$ char '-'
    plus = 1 <$ char '+'
    e = char 'e' <|> char 'E'

-- | Build a Double from its parts (sign, integral part, decimal part, exponent)
doubleFromParts :: Integer  -- sign
                -> Maybe Integer  -- integral part
                -> Maybe Double   -- decimal part
                -> Integer  -- exponent
                -> Maybe Double
doubleFromParts sign (Just int) (Just dec) expo =
  Just $ fromIntegral sign * (fromIntegral int + dec) * (10 ^^ expo)
doubleFromParts _ _ _ _ = Nothing

-- -- | Parser for json number values
jsonNumber :: Parser JsonValue
jsonNumber = do x <- doubleLiteral
                case x of
                    (Just n) -> return $ JsonNumber n
                    _        -> failure


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

--source: https://abhinavsarkar.net/posts/json-parsing-from-scratch-in-haskell/#jnumber-parser
showJSONString :: String -> String
showJSONString s = "\"" ++ concatMap showJSONChar s ++ "\""

--source: https://abhinavsarkar.net/posts/json-parsing-from-scratch-in-haskell/#jnumber-parser
isControl :: Char -> Bool
isControl c = c `elem` ['\0' .. '\31']

--source: https://abhinavsarkar.net/posts/json-parsing-from-scratch-in-haskell/#jnumber-parser
showJSONChar :: Char -> String
showJSONChar c = case c of
  '\'' -> "'"
  '\"' -> "\\\""
  '\\' -> "\\\\"
  '\b' -> "\\b"
  '\f' -> "\\f"
  '\n' -> "\\n"
  '\r' -> "\\r"
  '\t' -> "\\t"
  _ | isControl c -> "\\u" ++ showJSONNonASCIIChar c
  _ -> [c]
  where
    showJSONNonASCIIChar c =
      let a = "0000" ++ showHex (ord c) "" in drop (length a - 4) a


printJsonNull :: Int -> String
printJsonNull tabs = "null"

printJsonBool :: Bool -> Int -> String
printJsonBool (True) tabs = "true"
printJsonBool (False) tabs = "false"

printJsonNumber :: Double -> Int -> String
printJsonNumber f tabs = show f

printJsonArray :: [JsonValue] -> Int -> String
printJsonArray [] _ = []
printJsonArray [x] tabs = (replicate tabs '\t') ++ printJsonVal x tabs ++ "\n"
printJsonArray (x:xs) tabs  = (replicate tabs '\t') ++ (printJsonVal x tabs) ++ ",\n" ++ (printJsonArray xs tabs)

printJsonObject :: [(String, JsonValue)] -> Int -> String
printJsonObject [] _ = "\n"
printJsonObject [x] tabs = (replicate tabs '\t') ++ ("\"" ++ (fst x) ++ "\"") ++ " : " ++ (printJsonVal (snd x) tabs) ++ "\n"
printJsonObject (x:xs) tabs = (replicate tabs '\t') ++ ("\"" ++ (fst x) ++ "\"") ++ " : " ++ (printJsonVal (snd x) tabs) ++ ",\n" ++ (printJsonObject xs (tabs))

printJsonVal :: JsonValue -> Int -> String
printJsonVal (JsonNull) tabs = printJsonNull tabs   
printJsonVal (JsonBool t) tabs = printJsonBool t tabs   
printJsonVal (JsonNumber x) tabs = printJsonNumber x tabs  
printJsonVal (JsonString x) tabs = showJSONString x
printJsonVal (JsonArray x) tabs = "[\n" ++ (printJsonArray x (tabs+1)) ++ (replicate (tabs+1) '\t') ++ "]"  
printJsonVal (JsonObject x) tabs = "{\n" ++ (printJsonObject x (tabs+1)) ++ (replicate tabs '\t') ++ "}" 
