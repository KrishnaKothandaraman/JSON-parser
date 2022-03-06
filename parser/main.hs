import Json
import Parsing
import System.FilePath.Posix
import Data.Char hiding(isControl)
import Numeric
import Control.Applicative hiding(many)
import Text.Read(readMaybe)
import System.Directory

dumpParsedJson :: FilePath -> IO ()
dumpParsedJson inputFile = do inp <- readFile inputFile
                              writeFile (("../parsed_output/" ++ takeBaseName inputFile) ++ "_parsed" ++ ".json") (printJsonVal (fst $ head$ parse jsonValue inp) 0)

getFromFile :: FilePath -> IO String
getFromFile inputFile = do inp <- readFile inputFile
                           return inp

putToFile :: FilePath -> String -> IO ()
putToFile fpath str = do writeFile fpath str
                         return ()


processTests :: [FilePath] -> IO ()
processTests []     = return ()
processTests (x:xs) = do putStrLn $ "[Parsing file]: " ++ x
                         dumpParsedJson ("../test_cases/" ++ x)
                         processTests xs


getTestCases :: IO [FilePath]
getTestCases = do putStrLn "[Running Tests]"
                  x <- listDirectory "../test_cases"
                  return x

runTestCases :: IO ()
runTestCases  = do x <- getTestCases
                   processTests x
                  
main :: IO ()
main = undefined
