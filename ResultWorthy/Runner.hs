module ResultWorthy.Runner where

import ResultWorthy.DataTypes
import ResultWorthy.SwiftParsers(parseSwiftModule)
import ResultWorthy.SwiftRenderer
import ResultWorthy.Transforms
import ResultWorthy.ArgumentParser

import Data.Functor
import Data.Functor.Identity
import System.IO

import Text.Parsec.Error(ParseError)
import Text.PrettyPrint.Free

-- Make the Syntax Tree
processInput :: String -> Either ParseError SwiftModule
processInput = parseSwiftModule

-- Make the String Output
processOutput :: SwiftModule -> Mode -> String
processOutput mod Tree   = show mod
processOutput mod Render = process mod
  where process = show . pretty

-- Applies the Appropriate Transform
applyTransforms :: SwiftModule -> [Transform] -> SwiftModule
applyTransforms = foldr transform
  where transform ResultParse = extractResult
        transform OrderDecls  = orderDecls

-- TODO: This would be better with monad transformers
-- Takes the options and returns a SwiftModule inside an IO
runRead :: InputLocation -> IO SwiftModule
runRead input = do 
  contents <- readFromLocation input
  case processInput contents of
    Left msg -> fail $ show msg
    Right output -> return output

-- Reads a File from the given location
readFromLocation :: InputLocation -> IO String
readFromLocation (FileIn loc)  = readFile loc
readFromLocation StdIn         = getContents
readFromLocation (InParam str) = return str

-- Takes an output location and a module, writes it to the location
runWrite :: String -> OutputLocation -> IO ()
runWrite s StdOut        = putStrLn s
runWrite s (FileOut loc) = writeFile loc s

-- Takes Options and returns the result
runWithOptions :: Options -> IO String
runWithOptions options = do 
  swiftModule <- runRead (oInput options)
  let transformed = applyTransforms swiftModule (oTransforms options) 
  return $ processOutput transformed (oMode options)

-- The Purely-Side Efectful Variant
doRunWithOptions :: Options -> IO ()
doRunWithOptions options = runWithOptions options >>= (\x -> runWrite x (oOutput options))
