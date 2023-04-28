module Tests.MakeMove (makeMoveTests) where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Board
import           Utils


makeMoveTests :: TestTree
makeMoveTests = testGroup "makeMove"
  [ testCase "?" $ do
      let board = makeMove initialBoard X C3
      readBoard board C3 @?= X

  , testCase "?" $ do
      let board = makeMove initialBoard O C3
      readBoard board C3 @?= O
  ]
