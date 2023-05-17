module Tests.MakeMove (makeMoveTests) where

import           Test.Tasty
import           Test.Tasty.HUnit

import           TicTacTournament


makeMoveTests :: TestTree
makeMoveTests = testGroup "makeMove"
  [ testCase "?" $ do
      let board = makeMove emptyBoard X C3
      readBoard board C3 @?= X

  , testCase "?" $ do
      let board = makeMove emptyBoard O C3
      readBoard board C3 @?= O
  ]
