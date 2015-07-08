{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverlappingInstances #-}

module ResultWorthy.Tests.Renderer where

import ResultWorthy.Runner
import ResultWorthy.ArgumentParser
import ResultWorthy.SwiftRenderer
import ResultWorthy.DataTypes
import ResultWorthy.Util

import Data.Functor
import Data.List
import Data.Maybe
import Data.Algorithm.Diff
import System.Directory
import Test.HUnit
import Text.PrettyPrint.Free(pretty)

instance Show e => Show [(Int, Diff e)] where
  show = concatMap (\x -> show x ++ "\n")

instance Show e => Show (Int, Diff e) where
  show (index, diff) = show index ++ ": with difference " ++ show diff

instance (Eq e, Show e) => ListAssertable (Diff e) where
  listAssert = ass . filter pred . withIndeces
    where pred (_, Both _ _) = False
          pred (_, _)          = True
          ass []     = return ()
          ass xs = assertFailure $ "Expected no differences, got\n" ++ show xs

instance Assertable (String, String) where
  assert (left, right) = listAssert $ getDiff (lines left) (lines right)
    
instance Assertable (SwiftModule, SwiftModule) where
  assert (left, right) = assertEqual "The Trees arent equal" left right

findSwiftFiles :: FilePath -> IO [FilePath]
findSwiftFiles directory = map ((directory ++ "/") ++) . filter isSwift <$> getDirectoryContents directory
  where isSwift = isSuffixOf ".swift" :: String -> Bool

uroborosRender :: FilePath -> IO (String, String)
uroborosRender filePath = do 
  firstOutput <- runWithOptions $ Options (FileIn filePath) StdOut [] Render
  case processInput firstOutput of 
    Left msg -> fail $ show msg 
    Right secondTree -> do 
      let secondOutput = show $ pretty secondTree
      return (firstOutput, secondOutput)

uroborosTree :: FilePath -> IO (SwiftModule, SwiftModule)
uroborosTree filePath = do 
  firstTree <- runRead $ FileIn filePath
  let firstRender = show $ pretty firstTree
  secondTree <- runRead $ InParam firstRender
  return (firstTree, secondTree) 

assertParses :: FilePath -> Assertion
assertParses filePath = runRead (FileIn filePath) $> ()

assertUroborosRender :: FilePath -> Assertion
assertUroborosRender filePath = uroborosRender filePath >>= assert

assertUroborosTree :: FilePath -> Assertion
assertUroborosTree filePath = uroborosTree filePath >>= assert

makeParsesTests :: [FilePath] -> Test
makeParsesTests = TestLabel "The Tree Parses" . TestList . map individual
  where individual file = TestLabel ("Tree Parses for " ++ file) (TestCase $ assertParses file)

makeUroborosRenderTests :: [FilePath] -> Test
makeUroborosRenderTests = TestLabel "The Snake eats itself" . TestList . map individual
  where individual file = TestLabel ("The Snake eats itself for " ++ file) (TestCase $ assertUroborosRender file)

makeUroborosTreeTests :: [FilePath] -> Test
makeUroborosTreeTests = TestLabel "The Snake can see itself" . TestList . map individual
  where individual file = TestLabel ("The Snake can see itself for " ++ file) (TestCase $ assertUroborosTree file)

makeTests :: FilePath -> IO Test
makeTests directory = do
    files <- findSwiftFiles directory
    return $ TestLabel ("The Fixture Tests for " ++ concat files)
           $ TestList [
               makeParsesTests files,
               makeUroborosRenderTests files
               --makeUroborosTreeTests files
             ]

tests = makeTests "ResultWorthy/Tests/Fixtures"
