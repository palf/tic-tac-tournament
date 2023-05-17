module Main (main) where

import           Test.Tasty

import           Tests.Board
import           Tests.FindWinner
import           Tests.MakeMove
import           Tests.Position


main :: IO ()
main = defaultMain $ testGroup "game"
  [ positionTests, boardTests, findWinnerTests, makeMoveTests]
