{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tests.Board (boardTests) where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           Board


instance Arbitrary Transform where
  arbitrary = arbitraryBoundedEnum
  shrink None = []
  shrink _    = [None]


instance Arbitrary Sign where
  arbitrary = arbitraryBoundedEnum


instance Arbitrary Position where
  arbitrary = arbitraryBoundedEnum


instance Arbitrary Board where
  shrink = genericShrink
  arbitrary = Board <$> vectorOf 9 arbitrary


boardTests :: TestTree
boardTests = testGroup "board"
  [ positionFlipTests
  , positionRotateTests
  , positionTransformTests

  , boardFlipTests
  , boardRotateTests
  , boardTransformTests

  , boardKeyTests
  ]


positionFlipTests :: TestTree
positionFlipTests = testGroup "position flip"
  [ testCase "does not move the center column" $ do
      let position = A2
      identifyPosSource Flip position @?= position

  , testCase "swaps the left and right columns" $ do
      let position = A1
      let expected = A3
      identifyPosSource Flip position @?= expected
  ]


positionRotateTests :: TestTree
positionRotateTests = testGroup "position rotate"
  [ testCase "has no effect on center" $ do
      identifyPosSource Rot1 B2 @?= B2

  , testCase "rotates once clockwise" $ do
      identifyPosSource Rot1 A1 @?= C1
  ]


positionTransformTests :: TestTree
positionTransformTests = testGroup "position transforms"
  [ testProperty "can reverse an applied transform" $ \(transform :: Transform, position :: Position) ->
      (identifyPosTarget transform . identifyPosSource transform) position == position

  , testProperty "can reverse an applied transform" $ \(transform :: Transform, position :: Position) ->
      (identifyPosSource transform . identifyPosTarget transform) position == position
  ]


boardFlipTests :: TestTree
boardFlipTests = testGroup "board flip"
  [ testCase "has no effect when empty" $ do
      applyTransform Flip initialBoard @?= initialBoard

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
      applyTransform Rot1 initialBoard @?= initialBoard

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
       in key == "  OXXOOXX"

  ]

  where
    basicBoard = createBoard [Empty, Empty, O, X, X, O, O, X, X]
