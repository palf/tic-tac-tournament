module Main (main) where

import           Test.Tasty

import           Tests.Board
import           Tests.FindWinner
import           Tests.MakeMove


main :: IO ()
main = defaultMain $ testGroup "game"
  [ boardTests, findWinnerTests, makeMoveTests]


