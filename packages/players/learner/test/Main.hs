{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified Control.Monad.State     as State
import qualified Data.Map                as Map

import           Board
import           Control.Monad.State     (MonadState, StateT, evalStateT,
                                          execStateT, runStateT)
import           Data.Map                (Map, (!))
import           Players.Learner.Weights
import           Test.Tasty
import           Test.Tasty.HUnit


main :: IO ()
main = defaultMain allTests


allTests :: TestTree
allTests = testGroup "all" [ getWeightTests, modifyWeightTests ]


getWeightTests :: TestTree
getWeightTests = testGroup "getWeight"
  [ testCase "no rotation" $ do
      -- _ _ O
      -- O X X
      -- X O X
      getWeight weights testBoard A1 @?= 1.0
      getWeight weights testBoard A2 @?= 0.0

  , testCase "1 rotation" $ do
      -- O X X
      -- _ X O
      -- _ O X
      let board = createBoard [O, X, X, Empty, X, O, Empty, O, X]
      getWeight weights board A1 @?= 0.5
      getWeight weights board B1 @?= 0.0
      getWeight weights board C1 @?= 1.0

  , testCase "2 rotation" $ do
      -- X O X
      -- X X O
      -- O _ _
      let board = createBoard [X, O, X, X, X, O, O, Empty, Empty]
      getWeight weights board A1 @?= 0.5
      getWeight weights board C2 @?= 0.0
      getWeight weights board C3 @?= 1.0

  , testCase "3 rotation" $ do
      -- X O _
      -- O X _
      -- X X O
      let board = createBoard [X, O, Empty, O, X, Empty, X, X, O]
      getWeight weights board A1 @?= 0.5
      getWeight weights board A3 @?= 1.0
      getWeight weights board B3 @?= 0.0

  , testCase "flip" $ do
      -- O _ _
      -- X X O
      -- X O X
      let board = createBoard [O, Empty, Empty, X, X, O, X, O, X]
      getWeight weights board A1 @?= 0.5
      getWeight weights board A2 @?= 0.0
      getWeight weights board A3 @?= 1.0

  , testCase "1 rotation, flip" $ do
      -- X X O
      -- O X _
      -- X O _
      let board = createBoard [X, X, O, O, X, Empty, X, O, Empty]
      getWeight weights board A1 @?= 0.5
      getWeight weights board B3 @?= 0.0
      getWeight weights board C3 @?= 1.0

  , testCase "2 rotation, flip" $ do
      -- X O X
      -- O X X
      -- _ _ O
      let board = createBoard [X, O, X, O, X, X, Empty, Empty, O]
      getWeight weights board A1 @?= 0.5
      getWeight weights board C1 @?= 1.0
      getWeight weights board C2 @?= 0.0

  , testCase "3 rotation, flip" $ do
      -- _ O X
      -- _ X O
      -- O X X
      let board = createBoard [Empty, O, X, Empty, X, O, O, X, X]
      getWeight weights board A1 @?= 1.0
      getWeight weights board A2 @?= 0.5
      getWeight weights board B1 @?= 0.0
  ]

  where
    testBoard = createBoard [Empty, Empty, O, O, X, X, X, O, X]

    weights :: Weights
    weights = Map.fromList [ ( "  OOXXXOX", Map.fromList [ ("A1", 1.0) , ("A2", 0.0) ]) ]


instance (Monad m) => HasWeights (StateT Weights m) where
  readWeights = State.get
  writeWeights = State.put


modifyWeightTests :: TestTree
modifyWeightTests = testGroup "modifyWeight"
  [ testCase "no rotation" $ do
      -- _ _ O
      -- O X X
      -- X O X
      let board = createBoard [Empty, Empty, O, O, X, X, X, O, X]
      updatedWeights <- execStateT (takeWeight (board, A1) >> addWeight (board, A2)) weights

      Map.size (updatedWeights ! "  OOXXXOX" ) @?= 2
      getWeight updatedWeights board A1 @?= 0.9
      getWeight updatedWeights board A2 @?= 0.1

  , testCase "1 rotation" $ do
      -- O X X
      -- _ X O
      -- _ O X
      let board = createBoard [O, X, X, Empty, X, O, Empty, O, X]
      updatedWeights <- execStateT (takeWeight (board, C1) >> addWeight (board, B1)) weights

      Map.size (updatedWeights ! "  OOXXXOX" ) @?= 2
      getWeight updatedWeights board B1 @?= 0.1
      getWeight updatedWeights board C1 @?= 0.9

  , testCase "2 rotation" $ do
      -- X O X
      -- X X O
      -- O _ _
      let board = createBoard [X, O, X, X, X, O, O, Empty, Empty]
      updatedWeights <- execStateT (takeWeight (board, C3) >> addWeight (board, C2)) weights

      Map.size (updatedWeights ! "  OOXXXOX" ) @?= 2
      getWeight updatedWeights board C2 @?= 0.1
      getWeight updatedWeights board C3 @?= 0.9

  , testCase "3 rotation" $ do
      -- X O _
      -- O X _
      -- X X O
      let board = createBoard [X, O, Empty, O, X, Empty, X, X, O]
      updatedWeights <- execStateT (takeWeight (board, A3) >> addWeight (board, B3)) weights

      Map.size (updatedWeights ! "  OOXXXOX" ) @?= 2
      getWeight updatedWeights board B3 @?= 0.1
      getWeight updatedWeights board A3 @?= 0.9

  , testCase "flip" $ do
      -- O _ _
      -- X X O
      -- X O X
      let board = createBoard [O, Empty, Empty, X, X, O, X, O, X]
      updatedWeights <- execStateT (takeWeight (board, A3) >> addWeight (board, A2)) weights

      Map.size (updatedWeights ! "  OOXXXOX" ) @?= 2
      getWeight updatedWeights board A2 @?= 0.1
      getWeight updatedWeights board A3 @?= 0.9

  , testCase "1 rotation, flip" $ do
      -- _ O X
      -- _ X O
      -- O X X
      let board = createBoard [Empty, O, X, Empty, X, O, O, X, X]
      updatedWeights <- execStateT (takeWeight (board, A1) >> addWeight (board, B1)) weights

      Map.size (updatedWeights ! "  OOXXXOX" ) @?= 2
      getWeight updatedWeights board B1 @?= 0.1
      getWeight updatedWeights board A1 @?= 0.9

  , testCase "2 rotation, flip" $ do
      -- X O X
      -- O X X
      -- _ _ O
      let board = createBoard [X, O, X, O, X, X, Empty, Empty, O]
      updatedWeights <- execStateT (takeWeight (board, C1) >> addWeight (board, C2)) weights

      Map.size (updatedWeights ! "  OOXXXOX" ) @?= 2
      getWeight updatedWeights board C2 @?= 0.1
      getWeight updatedWeights board C1 @?= 0.9

  , testCase "3 rotation, flip" $ do
      -- X X O
      -- O X _
      -- X O _
      let board = createBoard [X, X, O, O, X, Empty, X, O, Empty]
      updatedWeights <- execStateT (takeWeight (board, C3) >> addWeight (board, B3)) weights

      Map.size (updatedWeights ! "  OOXXXOX" ) @?= 2
      getWeight updatedWeights board B3 @?= 0.1
      getWeight updatedWeights board C3 @?= 0.9

  ]

  where
    weights :: Weights
    weights = Map.fromList [ ( "  OOXXXOX", Map.fromList [ ("A1", 1.0) , ("A2", 0.0) ]) ]

    addWeight = modifyWeight (+ 0.1)
    takeWeight = modifyWeight (\x -> x - 0.1)
