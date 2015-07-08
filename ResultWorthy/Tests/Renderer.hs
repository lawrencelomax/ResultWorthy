module ResultWorthy.Tests.Renderer where

import ResultWorthy.Renderer

import Test.HUnit

assertLayout :: [FlexibleLayout String String] -> [FlexibleLayout String String] -> Assertion
assertLayout = assertEqual "The two layouts are not equal" 

tests = TestList [
    TestLabel "Reduces from the end" $ TestCase $ 
      assertLayout 
        [Content $ Just "a", Padding "b", Content $ Just "c"]
        (reduceLayout [Content $ Just "a", Padding "b", Content $ Just "c", Padding "d", Content Nothing]),
    TestLabel "Reduces from the beginning" $ TestCase $ 
      assertLayout 
        [Content $ Just "c", Padding "d", Content $ Just "e"]
        (reduceLayout [Content Nothing, Padding "b", Content $ Just "c", Padding "d", Content $ Just "e"]),
    TestLabel "Reduces trailing padding" $ TestCase $ 
      assertLayout
        [Content $ Just "a", Content $ Just "b"]
        (reduceLayout [Content $ Just "a", Content $ Just "b", Padding "c", Content Nothing, Padding "d"]),
    TestLabel "Reduces leading padding" $ TestCase $ 
      assertLayout
        [Content $ Just "c", Content $ Just "d"]
        (reduceLayout [Padding "a", Content Nothing, Padding "b", Content $ Just "c", Content $ Just "d"]),
    TestLabel "Reduces adjacent layouts" $ TestCase $ 
      assertLayout 
        [Content $ Just "a"]
        (reduceLayout [Content $ Just "a", Padding "b", Content Nothing, Padding "d", Content Nothing]),
    TestLabel "Reduces to nothing" $ TestCase $ 
      assertLayout 
        []
        (reduceLayout [Content Nothing, Padding "b", Content Nothing, Padding "d", Content Nothing])
  ]
