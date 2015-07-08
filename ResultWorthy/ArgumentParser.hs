module ResultWorthy.ArgumentParser where

import Options.Applicative
import Options.Applicative.Builder(info)
import Data.Monoid(mempty)
import Data.Maybe(catMaybes)

data Transform = ResultParse | OrderDecls deriving (Eq, Show, Read)
data InputLocation = StdIn | FileIn String | InParam String deriving (Eq, Show, Read)
data OutputLocation = StdOut | FileOut String deriving (Eq, Show, Read)
data Mode = Render | Tree deriving (Eq, Show, Read)

data Options = Options
  { oInput :: InputLocation
  , oOutput :: OutputLocation
  , oTransforms :: [Transform]
  , oMode :: Mode
  } deriving (Eq, Show, Read)

parser :: Parser Options
parser = Options <$> inputLocationParser <*> outputLocationParser <*> transformsParser <*> modeParser
  where modeParser = flag Render Tree (long "tree" <> help "Output the Syntax Tree")

transformsParser :: Parser [Transform]
transformsParser = construct <$> resultFlag <*> orderFlag
  where construct r o = catMaybes [r, o]
        resultFlag    = flag Nothing (Just ResultParse) (long "result" <> help "The Result Transform")
        orderFlag     = flag Nothing (Just OrderDecls) (long "order" <> help "The Ordering Transform")

outputLocationParser :: Parser OutputLocation
outputLocationParser = fmap FileOut fileParser <|> pure StdOut
  where fileParser = strOption (long "output" <> help "The Output File, if none is specified stdout is used")

inputLocationParser :: Parser InputLocation
inputLocationParser = fileParser <|> inParamParser <|> pure StdIn
  where fileParser    = FileIn <$> strOption 
                          (long "input" <> help "The Input File, if none is specified stdout is used")
        inParamParser = InParam <$> strOption 
                          (long "inputString" <> help "The Input, provided as a param")

opts :: ParserInfo Options
opts = info (helper <*> parser) 
  (fullDesc <> header "A Thing for screwing around with Swift")

