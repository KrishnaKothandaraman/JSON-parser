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
jsonString = do char '"'
                x <- many (sat (/='"'))
                char '"'
                return (JsonString x)


jsonNumber :: Parser JsonValue
jsonNumber = (do x <- many digit
                 char '.'
                 y <- many digit
                 return (JsonFloat $ (read :: String -> Float) (x++"."++y))) 
                 +++ 
                 (do x <- many digit
                     case x of 
                         "" -> failure
                         _  -> return (JsonInteger $ (read :: String -> Integer) x))

jsonBool :: Parser JsonValue
jsonBool = (do x <- string "true"
               return (JsonBool True)) +++
           (do x <- string "false"
               return (JsonBool False)
           )

jsonNull :: Parser JsonValue
jsonNull = do x <- string "null"
              return (JsonNull)

jsonArrayElement :: Parser JsonValue
jsonArrayElement = do char ','
                      x <- jsonValue
                      return x

jsonArray :: Parser JsonValue
jsonArray = do char '['
               x <- many jsonValue
               y <- many jsonArrayElement
               char ']'
               return (JsonArray (x++y))

pairs :: Parser (String, JsonValue)
pairs = do char '"' 
           key <- many1 alphanum
           token (char '"')
           token (char ':')
           value <- jsonValue
           token $ many (char ',')
           return (key, value)

jsonObject :: Parser JsonValue
jsonObject = do char '{'
                space
                x <- many pairs
                space
                char '}'
                return (JsonObject x) 

jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject

main :: IO()
main = undefined