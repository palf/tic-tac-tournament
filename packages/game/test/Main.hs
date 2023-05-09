module Main (main) where

import           Test.Tasty

import           Tests.Board
import           Tests.FindWinner
import           Tests.Game.Position
import           Tests.MakeMove


main :: IO ()
main = defaultMain $ testGroup "game"
  [ positionTests, boardTests, findWinnerTests, makeMoveTests]
