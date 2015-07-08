module ResultWorthy.Tests.SwiftParsers where

import ResultWorthy.SwiftParsers
import ResultWorthy.Parsers

import Test.HUnit
import Data.Either
import Text.Parsec.String

assertDidParse :: Parser a -> String -> Assertion
assertDidParse p s = assertBool ("Did Not Parse " ++ s) (isRight (parseString p s))

assertDidNotParse :: Parser a -> String -> Assertion
assertDidParse p s = assertBool ("Did Parse " ++ s) (isLeft (parseString p s))

-- Function Parsing
tests = TestList [
    TestLabel "Parses Functions" $ TestCase $ 
      assertDidParse functionDeclParser "func foo() -> Bool",
    TestLabel "Fails To Parse Bad Functions" $ TestCase $
      assertDidParse functionDeclParser "func fooa aa() -> Bool"
  ]
