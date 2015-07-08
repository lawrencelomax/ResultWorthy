module ResultWorthy.Tests.Util where

import ResultWorthy.Util
import Test.HUnit

tests = TestLabel "The Util Tests" $ TestList [
          TestLabel "withIndeces" $ TestCase 
            $ assertEqual "Arrays should be equal"
              (withIndeces ["foo", "bar", "baz", "bong"])
              [(0, "foo"), (1, "bar"), (2, "baz"), (3, "bong")]
          ,
          TestLabel "trimLeadingWhitespace" $ TestCase
            $ assertEqual "Output should make sense"
              [" foo", "bar", "       baz"]
              (trimLeadingWhitespace ["   foo", " bar", "         baz"])
        ]
