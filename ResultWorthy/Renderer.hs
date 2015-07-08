{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module ResultWorthy.Renderer where

import Data.Maybe
import Data.Functor
import Data.Functor.Identity
import Data.List(intersperse)
import Debug.Trace

import Text.PrettyPrint.Free

import ResultWorthy.Util

-- Like a Pretty, but with optionality
class Renderable a where
  render :: a -> Maybe (Doc e)
  renderList :: [a] -> Maybe (Doc e)
  renderList = undefined 

-- Data Type for Defining Layout and Spacing
data FlexibleLayout a b = Content (Maybe a) | Padding b
  deriving (Show, Eq, Read)

-- Removes redundant parts of the layout
reduceLayout :: (Show a, Show b) => [FlexibleLayout a b] -> [FlexibleLayout a b]
reduceLayout = filter fil . scanl (flip reducer) start . scanr reducer start
  where reducer (Padding _) (Content Nothing) = Content Nothing
        reducer a            _                = a
        fil (Content Nothing) = False
        fil _                 = True
        start = Content Nothing

-- Removes redundant spacing and documents, replacing it with the spaces.
joinLayout :: [FlexibleLayout (Doc e) (Doc e)] -> Maybe (Doc e)
joinLayout layout = hcat <$> emptyMaybe (mapMaybe pullout layout)
  where pullout (Padding i) = Just i 
        pullout (Content a) = a

-- condRender x y b, renders x if b is True, y if b is false
condRender :: Doc e -> Doc e -> Bool -> Doc e
condRender t _ True  = t
condRender _ f False = f

-- renderTrue t b, renders t if b is True, empty doc otherwise
renderTrue :: Doc e -> Bool -> Doc e
renderTrue t = condRender t empty

-- Wraps a Doc e as the body of a block
blockBody :: Doc e -> Doc e
blockBody b = lbrace <> line <> indent 4 b <> line <> rbrace

-- Horizontally intersperses
separate :: Doc e -> [Doc e] -> Doc e
separate s docs = hcat $ punctuate s docs

-- Horizontally intersperses, removing reduntant parts
-- Returns nothing if there is no layout
separateMaybe :: Doc e -> [Maybe (Doc e)] -> Maybe (Doc e)
separateMaybe sep docs =
  let layout = intersperse (Padding sep) $ map Content docs
  in  joinLayout $ reduceLayout layout

-- A Doc of i Spaces
spaces :: Int -> Doc e
spaces count = hcat $ replicate count space

-- Separates the Joinables by a comma, then space
commaSep :: [Doc e] -> Doc e
commaSep = separate (text ", ")

-- Comma Separate with parens
parensCommaSep :: [Doc e] -> Doc e
parensCommaSep = parens . commaSep

-- Separates Joinable by Spaces, of a given number
spaceSep :: Int -> [Doc e] -> Doc e
spaceSep count = separate $ spaces count

-- Separates a Joinable by spaces
spaceSepMaybe :: Int -> [Maybe (Doc e)] -> Maybe (Doc e)
spaceSepMaybe count = separateMaybe $ spaces count

-- Separates Joinables by Lines, of a given number
lineSep :: Int -> [Doc e] -> Doc e
lineSep count docs = 
  let separator = hcat $ replicate count (text "\n")
  in  separate separator docs
