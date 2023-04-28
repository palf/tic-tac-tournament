module Tests.FindWinner (findWinnerTests) where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Board
import           Utils


findWinnerTests :: TestTree
findWinnerTests = testGroup "findWinner"
  [ testCase "initial no winner" $ do
    findWinner initialBoard @?= Nothing

  , testCase "testboard no winner" $ do
    let board = setBoard
          [ (A1, X), (A2, O), (A3, X)
          , (B1, O)
          , (C1, O), (C2, X)
          ]

    findWinner board @?= Nothing

  , testCase "row 01 winner" $ do
    let board = setBoard [ (A1, X), (A2, X), (A3, X) ]
    findWinner board @?= Just X

  , testCase "row 02 winner" $ do
    let board = setBoard [ (B1, O), (B2, O), (B3, O) ]
    findWinner board @?= Just O

  , testCase "row 03 winner" $ do
    let board = setBoard [ (C1, X), (C2, X), (C3, X) ]
    findWinner board @?= Just X

  , testCase "col 01 winner" $ do
    let board = setBoard [ (A1, O), (B1, O), (C1, O) ]
    findWinner board @?= Just O

  , testCase "col 02 winner" $ do
    let board = setBoard [ (A2, X), (B2, X), (C2, X) ]
    findWinner board @?= Just X

  , testCase "col 03 winner" $ do
    let board = setBoard [ (A3, O), (B3, O), (C3, O) ]
    findWinner board @?= Just O

  , testCase "diag 01 winner" $ do
    let board = setBoard [ (A1, X), (B2, X), (C3, X) ]
    findWinner board @?= Just X

  , testCase "diag 02 winner" $ do
    let board = setBoard [ (C1, O), (B2, O), (A3, O) ]
    findWinner board @?= Just O

  , testCase "unfinished" $ do
      -- X O X
      -- _ X _
      -- _ O O
      let board = setBoard [ (A1, X) , (A2, O) , (A3, X) , (B2, X) , (C2, O) , (C3, O) ]
      findWinner board @?= Nothing

  , testCase "assess 02" $ do
      -- X O X
      -- _ X _
      -- X O O
      let board = setBoard [ (A1, X) , (A2, O) , (A3, X) , (B2, X) , (C1, X) , (C2, O) , (C3, O) ]

      findWinner board @?= Just X

  , testCase "assess 04" $ do
      -- X O X
      -- X X _
      -- O O O
      let board = setBoard [ (A1, X) , (A2, O) , (A3, X) , (B1, X) , (B2, X) , (C1, O) , (C2, O) , (C3, O) ]

      findWinner board @?= Just O

  , testCase "assess 05" $ do
      -- X O X
      -- _ X X
      -- O O O
      let board = setBoard [ (A1, X) , (A2, O) , (A3, X) , (B2, X) , (B3, X) , (C1, O) , (C2, O) , (C3, O) ]

      findWinner board @?= Just O

  ]
