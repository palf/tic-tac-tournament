{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified Data.Map                as Map

import           Board
import           Players.Learner.Weights
import           Test.Tasty
import           Test.Tasty.HUnit


main :: IO ()
main = defaultMain allTests


allTests :: TestTree
allTests = testGroup "all" [ getWeightTests ]


getWeightTests :: TestTree
getWeightTests = testGroup "getWeight"
  [ testCase "no rotation" $ do
      -- _ _ O
      -- O X X
      -- X O X
      let board = createBoard [Empty, Empty, O, O, X, X, X, O, X]
      let boardString = "  OOXXXOX"

      let (weights :: Weights) = Map.fromList [ (boardString, positionWeights) ]

      getWeight weights board A1 @?= 0.1
      getWeight weights board A2 @?= 0.2
      getWeight weights board B2 @?= 0.6

  , testCase "1 rotation" $ do
      -- _ O X
      -- _ X X
      -- _ O O
      let board = createBoard [Empty, O, X, Empty, X, X, Empty, O, O]
      let boardString = "   OXOOXX"

      let (weights :: Weights) = Map.fromList [ (boardString, positionWeights) ]
      let value = getWeight weights board A1

      value @?= 0.8

  , testCase "2 rotation" $ do
      -- O _ _
      -- _ X _
      --   O _
      let board = createBoard [O, Empty, Empty, Empty, X, Empty, Empty, O, Empty]
      let boardString = "    XOO  "

      let (weights :: Weights) = Map.fromList [ (boardString, positionWeights) ]
      let value = getWeight weights board A1

      value @?= 0.8

  , testCase "3 rotation" $ do
      -- O _ _
      -- _ X _
      --   O _
      let board = createBoard [O, Empty, Empty, Empty, X, Empty, Empty, O, Empty]
      let boardString = "    XOO  "

      let (weights :: Weights) = Map.fromList [ (boardString, positionWeights) ]
      let value = getWeight weights board A1

      value @?= 0.8
  ]

  where
      positionWeights = Map.fromList
        [ ("A1", 0.1)
        , ("A2", 0.2)
        , ("A3", 0.3)
        , ("B1", 0.4)
        , ("B2", 0.6)
        , ("B3", 0.7)
        , ("C1", 0.8)
        , ("C2", 0.9)
        , ("C3", 1.0)
        ]
