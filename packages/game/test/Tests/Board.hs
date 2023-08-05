{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tests.Board (boardTests) where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           TicTacTournament


instance Arbitrary Transform where
  arbitrary = arbitraryBoundedEnum
  shrink = genericShrink


instance Arbitrary Sign where
  arbitrary = arbitraryBoundedEnum


instance Arbitrary Position where
  arbitrary = arbitraryBoundedEnum


instance Arbitrary Board where
  arbitrary = createBoard <$> vectorOf 9 arbitrary


boardTests :: TestTree
boardTests = testGroup "board"
  [ boardFlipTests
  , boardRotateTests
  , boardTransformTests
  , boardKeyTests
  ]


boardFlipTests :: TestTree
boardFlipTests = testGroup "board flip"
  [ testCase "has no effect when empty" $ do
      applyTransform Flip emptyBoard @?= emptyBoard

  , testCase "does not move the center column" $ do
      let board = createBoard [ Empty, X, Empty, O, Empty, O, X, O, X]
      applyTransform Flip board @?= board

  , testCase "swaps the left and right columns" $ do
      let board = createBoard [ X, Empty, O, Empty, O, X, O, X, Empty]
      let expected = createBoard [ O, Empty, X, X, O, Empty, Empty, X, O]
      applyTransform Flip board @?= expected
  ]


boardRotateTests :: TestTree
boardRotateTests = testGroup "board rotate"
  [ testCase "has no effect when empty" $ do
      applyTransform Rot1 emptyBoard @?= emptyBoard

  , testCase "rotates once clockwise" $ do
      let board = createBoard [ X, Empty, Empty, Empty, O, X, O, X, Empty]
      let expected  = createBoard [ O, Empty, X, X, O, Empty, Empty, X, Empty]
      applyTransform Rot1 board @?= expected
  ]


boardTransformTests :: TestTree
boardTransformTests = testGroup "board transforms"
  [ testProperty "changes the board" $ \(x :: Transform) ->
      x /= None ==> applyTransform x testBoard /= testBoard

  , testProperty "can reverse an applied transform" $ \(x :: Transform, board :: Board) ->
      revertTransform x (applyTransform x board) == board
  ]

  where
    -- O _ X
    -- X O _
    -- _ O X
    testBoard = createBoard [ O, Empty, X, X, O, Empty, Empty, O, X]



boardKeyTests :: TestTree
boardKeyTests = testGroup "board key"
  [ testProperty "all transforms return the same board key" $ \(transform :: Transform) ->
      let board = applyTransform transform basicBoard
          (key, _) = getBoardKey board
       in key == BoardKey "  O|XXO|OXX"

  , testCase "key is always the alphabetically-first representation of a board" $ do
      let strings = (\t -> BoardKey $ boardToText $ applyTransform t basicBoard ) <$> [None, Rot1, Rot2, Rot2, Flip, FlipRot1, FlipRot2, FlipRot3]
          best = minimum strings
          (key, _) = getBoardKey basicBoard

      key @?= best

  ]

  where
    basicBoard = createBoard [Empty, Empty, O, X, X, O, O, X, X]
