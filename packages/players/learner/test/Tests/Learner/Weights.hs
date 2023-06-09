{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tests.Learner.Weights (weightTests) where

import qualified Data.Aeson                 as Aeson
import qualified Data.Map                   as Map

import           Control.Monad.State.Strict (execStateT, runStateT)
import           Data.Map                   ((!))
import           Players.Learner.Actions
import           Players.Learner.Weights
import           Test.Tasty
import           Test.Tasty.HUnit
import           TicTacTournament



weightTests :: TestTree
weightTests = testGroup "weights" [ getWeightTests, modifyWeightTests, jsonTests, towardTests ]


getWeightTests :: TestTree
getWeightTests = testGroup "getWeight"
  [ testCase "no rotation" $ do
      -- _ _ O
      -- O X X
      -- X O X
      let ps = [A1, A2, A3, B1, B2, B3, C1, C2, C3]
      let expected = [1.0, 0.0, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5]
      getWeight weights testBoard <$> ps @?= expected

  , testCase "1 rotation" $ do
      -- X O _
      -- O X _
      -- X X O
      let board = createBoard [X, O, Empty, O, X, Empty, X, X, O]
      getWeight weights board A3 @?= 1.0
      getWeight weights board B3 @?= 0.0
      getWeight weights board C3 @?= 0.5

  , testCase "2 rotations" $ do
      -- X O X
      -- X X O
      -- O _ _
      let board = createBoard [X, O, X, X, X, O, O, Empty, Empty]
      getWeight weights board C1 @?= 0.5
      getWeight weights board C2 @?= 0.0
      getWeight weights board C3 @?= 1.0

  , testCase "3 rotations" $ do
      -- O X X
      -- _ X O
      -- _ O X
      let board = createBoard [O, X, X, Empty, X, O, Empty, O, X]
      getWeight weights board A1 @?= 0.5
      getWeight weights board B1 @?= 0.0
      getWeight weights board C1 @?= 1.0

  , testCase "flip" $ do
      -- O _ _
      -- X X O
      -- X O X
      let board = createBoard [O, Empty, Empty, X, X, O, X, O, X]
      getWeight weights board A1 @?= 0.5
      getWeight weights board A2 @?= 0.0
      getWeight weights board A3 @?= 1.0

  , testCase "flip, 1 rotation" $ do
      -- X X O
      -- O X _
      -- X O _
      let board = createBoard [X, X, O, O, X, Empty, X, O, Empty]
      getWeight weights board A1 @?= 0.5
      getWeight weights board B3 @?= 0.0
      getWeight weights board C3 @?= 1.0

  , testCase "flip, 2 rotations" $ do
      -- X O X
      -- O X X
      -- _ _ O
      let board = createBoard [X, O, X, O, X, X, Empty, Empty, O]
      getWeight weights board A1 @?= 0.5
      getWeight weights board C1 @?= 1.0
      getWeight weights board C2 @?= 0.0

  , testCase "flip, 3 rotations" $ do
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
    weights = Map.fromList [ ( BoardKey "  O|OXX|XOX", Map.fromList [ (A1, 1.0) , (A2, 0.0) ]) ]


modifyWeightTests :: TestTree
modifyWeightTests = testGroup "modifyWeight"
  [ testCase "no rotation" $ do
      -- _ _ O
      -- O X X
      -- X O X
      let board = createBoard [Empty, Empty, O, O, X, X, X, O, X]
      updatedWeights <- execStateT (decreaseWeight (board, A1) >> increaseWeight (board, A2) >> increaseWeight (board, A3)) weights

      Map.size (updatedWeights ! BoardKey "  O|OXX|XOX" ) @?= 3
      getWeight updatedWeights board A1 @?= 0.9
      getWeight updatedWeights board A2 @?= 0.1
      getWeight updatedWeights board A3 @?= 0.6

  , testCase "1 rotation" $ do
      -- O X X
      -- _ X O
      -- _ O X
      let board = createBoard [O, X, X, Empty, X, O, Empty, O, X]
      updatedWeights <- execStateT (decreaseWeight (board, C1) >> increaseWeight (board, B1)) weights

      Map.size (updatedWeights ! BoardKey "  O|OXX|XOX" ) @?= 2
      getWeight updatedWeights board B1 @?= 0.1
      getWeight updatedWeights board C1 @?= 0.9

  , testCase "2 rotations" $ do
      -- X O X
      -- X X O
      -- O _ _
      let board = createBoard [X, O, X, X, X, O, O, Empty, Empty]
      updatedWeights <- execStateT (decreaseWeight (board, C3) >> increaseWeight (board, C2)) weights

      Map.size (updatedWeights ! BoardKey "  O|OXX|XOX" ) @?= 2
      getWeight updatedWeights board C2 @?= 0.1
      getWeight updatedWeights board C3 @?= 0.9

  , testCase "3 rotations" $ do
      -- X O _
      -- O X _
      -- X X O
      let board = createBoard [X, O, Empty, O, X, Empty, X, X, O]
      updatedWeights <- execStateT (decreaseWeight (board, A3) >> increaseWeight (board, B3)) weights

      Map.size (updatedWeights ! BoardKey "  O|OXX|XOX" ) @?= 2
      getWeight updatedWeights board B3 @?= 0.1
      getWeight updatedWeights board A3 @?= 0.9

  , testCase "flip" $ do
      -- O _ _
      -- X X O
      -- X O X
      let board = createBoard [O, Empty, Empty, X, X, O, X, O, X]
      updatedWeights <- execStateT (decreaseWeight (board, A3) >> increaseWeight (board, A2)) weights

      Map.size (updatedWeights ! BoardKey "  O|OXX|XOX" ) @?= 2
      getWeight updatedWeights board A2 @?= 0.1
      getWeight updatedWeights board A3 @?= 0.9

  , testCase "flip, 1 rotation" $ do
      -- _ O X
      -- _ X O
      -- O X X
      let board = createBoard [Empty, O, X, Empty, X, O, O, X, X]
      updatedWeights <- execStateT (decreaseWeight (board, A1) >> increaseWeight (board, B1)) weights

      Map.size (updatedWeights ! BoardKey "  O|OXX|XOX" ) @?= 2
      getWeight updatedWeights board B1 @?= 0.1
      getWeight updatedWeights board A1 @?= 0.9

  , testCase "flip, 2 rotations" $ do
      -- X O X
      -- O X X
      -- _ _ O
      let board = createBoard [X, O, X, O, X, X, Empty, Empty, O]
      updatedWeights <- execStateT (decreaseWeight (board, C1) >> increaseWeight (board, C2)) weights

      Map.size (updatedWeights ! BoardKey "  O|OXX|XOX" ) @?= 2
      getWeight updatedWeights board C2 @?= 0.1
      getWeight updatedWeights board C1 @?= 0.9

  , testCase "flip, 3 rotations" $ do
      -- X X O
      -- O X _
      -- X O _
      let board = createBoard [X, X, O, O, X, Empty, X, O, Empty]
      updatedWeights <- execStateT (decreaseWeight (board, C3) >> increaseWeight (board, B3)) weights

      Map.size (updatedWeights ! BoardKey "  O|OXX|XOX" ) @?= 2
      getWeight updatedWeights board B3 @?= 0.1
      getWeight updatedWeights board C3 @?= 0.9

  , testCase "returns the updated value" $ do
      let board = createBoard [X, X, O, O, X, Empty, X, O, Empty]
      (x, _) <- runStateT (decreaseWeight (board, C3)) weights

      x @?= 0.9
  ]

  where
    weights :: Weights
    weights = Map.fromList [ ( BoardKey "  O|OXX|XOX", Map.fromList [ (A1, 1.0) , (A2, 0.0) ]) ]

    increaseWeight (b, s) = modifyWeight (+ 0.1) (Exploit, b, s)
    decreaseWeight (b, s) = modifyWeight (\x -> x - 0.1) (Exploit, b, s)



jsonTests :: TestTree
jsonTests = testGroup "json"
  [ testCase "encode" $ do
      Aeson.encode weights @?= "{\"  O|OXX|XOX\":{\"A1\":1.0,\"A2\":0.0}}"

  , testCase "decode" $ do
      Aeson.eitherDecode "{\"  O|OXX|XOX\":{\"A1\":1.0,\"A2\":0.0}}" @?= Right weights

  , testCase "both" $ do
      Aeson.eitherDecode (Aeson.encode weights) @?= Right weights
  ]

  where
    weights :: Weights
    weights = Map.fromList [ ( BoardKey "  O|OXX|XOX", Map.fromList [ (A1, 1.0) , (A2, 0.0) ]) ]



towardTests :: TestTree
towardTests = testGroup "towards"
  [ testCase "moves halfway toward the target" $ toward target 0.5 @?= 0.75
  , testCase "does not pass the target" $ toward target target @?= target
  ]

 where
   target = 1 :: Float
