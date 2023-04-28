module Main (main) where

import           Test.Tasty

import           Tests.Players.Minimax


main :: IO ()
main = defaultMain $ testGroup "minimax" [playerMinimaxTests]
