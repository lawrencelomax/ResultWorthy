module ResultWorthy.Util where

import Data.Maybe
import Data.Char(isSpace)
import Data.List
import Data.Tuple
import Data.Functor.Identity

-- Takes a list, returns nothing if empty, Just the list otherwise
emptyMaybe :: [a] -> Maybe [a]
emptyMaybe [] = Nothing
emptyMaybe ls = Just ls

-- TODO: Try this with monad transformers
fromIdentity :: Identity a -> Maybe a
fromIdentity = return . runIdentity

-- Adds indeces to a List
withIndeces :: [a] -> [(Int, a)]
withIndeces = snd . mapAccumL reducer 0
  where reducer index value = (index + 1, (index, value))

-- Removes the minimal trailing whitespace across lines
trimLeadingWhitespace :: [String] -> [String]
trimLeadingWhitespace [] = []
trimLeadingWhitespace xs = stripIt (length (foldr1 reducer xs)) xs
  where stripIt i = map (drop i)
        trimmable = length . takeWhile isSpace
        reducer l p
            | trimmable l < trimmable p = l 
            | otherwise = p
