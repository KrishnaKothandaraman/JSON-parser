module Main where
import Parsing

data JsonValue = JsonNull
                 | JsonBool Bool 
                 | JsonNumber Integer -- TODO: Implement Float
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
jsonNumber = do x <- many digit
                return (JsonNumber $ (read :: String -> Integer) x)

jsonBool :: Parser JsonValue
jsonBool = (do x <- string "true"
               return (JsonBool True)) +++
           (do x <- string "false"
               return (JsonBool False)
           )

jsonNull :: Parser JsonValue
jsonNull = do x <- string "null"
              return (JsonNull)

jsonValue :: Parser JsonValue
jsonValue = undefined

main :: IO()
main = undefined