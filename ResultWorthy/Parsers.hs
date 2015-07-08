module ResultWorthy.Parsers where

import Data.Maybe
import Data.Functor
import Data.Either
import Data.Traversable

import Control.Applicative((*>), (<*), (<*>))
import Control.Monad

import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec (ParseError)

-- Take a Parser and a String, then Parse it
parseString :: Parser a -> String -> Either ParseError a
parseString p = parse p ""

-- Take a Parser and a String, the Parse it and return the rest of the string
parseWithLeftOver :: Parser a -> String -> Either ParseError (a,String)
parseWithLeftOver p = parse ((,) <$> p <*> leftOver) ""
  where leftOver = manyTill anyToken eof

-- A 3 Arg Version of curry
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

-- Converts a Tuple of Monads to a Monad of Tuples
unwrapM :: Monad m => (m a, m b) -> m (a, b)
unwrapM = uncurry $ liftM2 (,)

-- A 3-Tuple version of unwrapM
unwrapM3 :: Monad m => (m a, m b, m c) -> m (a, b, c)
unwrapM3 = uncurry3 $ liftM3 (,,)

-- Parses either of the two input parsers, with preference towards the first
-- This is distinct from `choice`, which requires all functors to be of the same type
parseEither :: Parser a -> Parser b -> Parser (Either a b)
parseEither a b = fmap Left a <|> fmap Right b

-- Parses each of the inputs repeatedly. Each of the parsers can be applied, preference is given to the first
accumilateEither :: Parser a -> Parser b -> Parser ([a], [b])
accumilateEither a b = fmap partitionEithers (many eitherP) 
  where eitherP = parseEither a b

-- A 3-Arg version of accumilateEither
accumilateEither3 :: Parser a -> Parser b -> Parser c -> Parser ([a], [b], [c])
accumilateEither3 a b c = fmap restruct (many eitherP)
  where eitherP               = parseEither (parseEither a b) c
        restruct1 ((a, b), c) = (a, b, c)
        restruct0 (ab, c)     = (partitionEithers ab, c)
        restruct              = restruct1 . restruct0 . partitionEithers 

-- A 4-Arg version of accumilateEither
accumilateEither4 :: Parser a -> Parser b -> Parser c -> Parser d -> Parser ([a], [b], [c], [d])
accumilateEither4 a b c d = fmap restruct (many eitherP)
  where eitherP                    = parseEither (parseEither a b) (parseEither c d)
        restruct1 ((a, b), (c, d)) = (a, b, c, d) 
        restruct0 (ab, cd)         = (partitionEithers ab, partitionEithers cd)
        restruct                   = restruct1 . restruct0 . partitionEithers

-- A 5-Arg version of accumilateEither
accumilateEither5 :: Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser ([a], [b], [c], [d], [e])
accumilateEither5 a b c d e = fmap restruct (many eitherP)
 where eitherP                         = parseEither (parseEither (parseEither a b) (parseEither c d)) e
       restruct2 (((a, b), (c, d)), e) = (a, b, c, d, e)
       restruct1 ((ab, cd), e)         = ((partitionEithers ab, partitionEithers cd), e)
       restruct0 (abcd, e)             = (partitionEithers abcd, e)
       restruct                        = restruct2 . restruct1 . restruct0 . partitionEithers

-- Just bang an eof on the end
accumilateEitherEof :: Parser a -> Parser b -> Parser ([a], [b])
accumilateEitherEof a b = fmap partitionEithers (manyTill eitherP eof) 
  where eitherP = parseEither a b

-- Parses with the first parser, then the second, if the second succeeds, apply the function
followOn :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
followOn m x y = do xp <- x
                    yp <- y
                    return $ m xp yp

-- Parses with the first parser, then the second if the second succeed
followOnOptional :: (a -> Maybe b -> c) -> Parser a -> Parser b -> Parser c
followOnOptional m x y = followOn m x (optionMaybe y)

-- Applies the first parser, then the second, mappending the result of the first to the result of the second
beforeAppend :: Parser [a] -> Parser [a] -> Parser [a]
beforeAppend = followOn (++)

-- Parses the input parser, ignoring leading and trailing whitespace
padded :: Parser a -> Parser a
padded = between spaces spaces

-- Parses the input parser, if is surrounded by brackets
parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

-- Parses the input parser, if it is surrounded by curly braces
braced :: Parser a -> Parser a
braced = between (char '{') (char '}')

-- Parses the input parser, surrounded by angle braces
angleBraced :: Parser a -> Parser a
angleBraced = between (char '<') (char '>')

-- Parses the input parser, surrouted by square braces
squareBraced :: Parser a -> Parser a
squareBraced = between (char '[') (char ']')

-- Returns True if the input parsers succeeds, False otherwise
doesMatch :: Parser a -> Parser Bool
doesMatch p = (p $> True) <|> return False

-- Returns a1 if the parse succeeds, a2 otherwise
matchConst :: Parser a -> b -> b -> Parser b
matchConst p a1 a2 = mapper <$> doesMatch p
  where mapper True  = a1
        mapper False = a2

-- (Parses, these, kinds,of things) and ()
parensCommaSep :: Parser a -> Parser [a]
parensCommaSep = parens . flip sepBy (char ',')

-- (Parses, these, kinds,of things) but not ()
parensCommaSep1 :: Parser a -> Parser [a]
parensCommaSep1 = parens . flip sepBy1 (char ',')

newtype ContentTree a = ContentTree [Either a (ContentTree a)]
  deriving (Show, Read, Eq)

--instance Traversable ContentTree where
--  traverse f (ContentTree list) = traverse (\e -> traverse f e) list

-- Recursively Parses opening and closing in a tree. 
-- Suitable for parsing nested parens, where simple `between` will result in early termination of the parser
-- TODO: A better way of closing over the open and close parens without confusing the compiler
parseTree :: Char -> Char -> Parser (ContentTree String)
parseTree open close = 
    let node = parseTree open close
        body = many1 (noneOf [open, close])
    in  ContentTree <$> between (char open) (char close) (many1 (parseEither body node))

-- Extracts all of the elements of the tree
flattenTree :: a -> a -> ContentTree a -> [a]
flattenTree open close (ContentTree ls) =
  let reduce (Left a)   p  = p ++ [a]
      reduce (Right ct) p  = p ++ [open] ++ flattenTree open close ct ++ [close]
  in  foldr reduce [] ls

-- Recusively Parses between the opening and closing arguments
--recursiveParse :: Parser a -> Parser b -> Parser c -> Tree a
--recursiveParse a b c = 
--  let node = Node <$> between a b (many recursiveParse)
--      leaf = Leaf <$> many1 (oneOf a <|> b)
--  in  node <|> leaf

