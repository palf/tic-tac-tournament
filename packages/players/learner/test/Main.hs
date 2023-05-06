{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Tests.Learner.Weights


main :: IO ()
main = defaultMain allTests


allTests :: TestTree
allTests = testGroup "learner" [ weightTests ]
