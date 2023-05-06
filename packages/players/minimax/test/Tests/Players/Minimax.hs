module Tests.Players.Minimax (playerMinimaxTests) where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Board
import           Players.Minimax


playerMinimaxTests :: TestTree
playerMinimaxTests = testGroup "minimax"
  [ testCase "win is greater than loss" $ do
      WinForX > WinForO @? "win beats loss"
      WinForX > Draw @? "win beats draw"
      Draw > WinForO @? "draw beats loss"

  , testCase "assess 06" $ do
      -- X O X
      -- O _ _
      -- O X _
      let board = createBoard [ X, O, X, O, Empty, Empty, O, X, Empty ]
      assess X board @?= [(B2, Draw), (B3, Draw), (C3, WinForX)]

  , testCase "assess 07" $ do
      -- X X O
      -- X _ _
      -- O O _
      let board = createBoard [ X , X , O , X , Empty, Empty, O , O, Empty ]
      assess X board @?= [(B2, WinForO), (B3, WinForO), (C3, WinForO)]

  , testCase "assess 08" $ do
      -- X O X
      -- _ _ _
      -- _ O _
      let board = createBoard [ X, O, X, Empty, Empty, Empty, Empty, O, Empty ]
      assess X board @?= [(B1, WinForO), (B2, WinForX), (B3, WinForO), (C1, WinForO), (C3, WinForO)]

  , testCase "assess 09" $ do
      -- X O X
      -- _ X _
      -- _ O O
      let board = createBoard [ X, O, X, Empty, X, Empty, Empty, O, O ]
      assess X board @?= [(B1, WinForO), (B3, WinForO), (C1, WinForX)]

  , testCase "optimiseFor 09" $ do
      -- X O X
      -- _ X _
      -- _ O O
      let board = createBoard [ X, O, X, Empty, X, Empty, Empty, O, O ]
      mpos <- optimiseFor X board
      mpos @?= Just C1

  , testCase "assess 10" $ do
      -- X O X
      -- O X _
      -- _ O _
      let board = createBoard [ X, O, X, O, X, Empty, Empty, O, Empty ]
      assess X board @?= [ (B3, WinForX), (C1, WinForX), (C3, WinForX) ]

  , testCase "assess 12" $ do
      -- X O X
      -- O O X
      -- _ _ _
      let board = createBoard [ X, O, X, O, O, X, Empty, Empty, Empty ]
      assess X board @?= [ (C1, WinForO), (C2, Draw), (C3, WinForX) ]

      mpos <- optimiseFor X board
      mpos @?= Just C3

  , testCase "assess 13" $ do
      -- X O O
      -- X _ _
      -- _ _ _
      let board = createBoard [ X, O, O, X, Empty, Empty, Empty, Empty, Empty ]

      assess X board @?=
        [ (B2, WinForX), (B3, WinForX), (C1, WinForX), (C2, WinForX), (C3, WinForX) ]

  , testCase "assess 13" $ do
      -- X O O
      -- X _ _
      -- _ X _
      let board = createBoard [ X, O, O, X, Empty, Empty, Empty, X, Empty ]

      assess O board @?=
        [ (B2, WinForX), (B3, WinForX), (C1, WinForX), (C3, WinForX) ]

  , testCase "assess 13.1" $ do
      -- X O O
      -- X _ _
      -- O X _
      let board = createBoard [ X, O, O, X, Empty, Empty, O, X, Empty ]

      assess X board @?=
        [ (B2, WinForX), (B3, WinForO), (C3, WinForO) ]

  , testCase "assess 14" $ do
      -- X _ _
      -- _ _ O
      -- _ _ _
      let board = createBoard [ X, Empty, Empty, Empty, Empty, O, Empty, Empty, Empty ]

      assess X board @?=
        [ (A2, WinForO)
        , (A3, WinForX)
        , (B1, Draw)
        , (B2, WinForX)
        , (C1, WinForX)
        , (C2, Draw)
        , (C3, Draw)
        ]

  , testCase "assess 15" $ do
      -- _ _ _
      -- _ _ X
      -- O O X
      let board = createBoard [ Empty, Empty, Empty, Empty, Empty, X, O, O, X ]

      assess X board @?=
        [ (A1, WinForX)
        , (A2, WinForX)
        , (A3, WinForX)
        , (B1, WinForX)
        , (B2, WinForX)
        ]

  ]
