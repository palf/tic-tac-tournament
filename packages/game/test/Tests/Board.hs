{-# OPTIONS_GHC -fno-warn-orphans #-}

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
  arbitrary
    = Board
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary


boardTests :: TestTree
boardTests = testGroup "board"
  [ flipTests
  , rotateTests
  , boardKeyTests
  , boardTransformTests
  , positionTransformTests
  ]


flipTests :: TestTree
flipTests = testGroup "flip"
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


rotateTests :: TestTree
rotateTests = testGroup "rotate"
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


positionTransformTests :: TestTree
positionTransformTests = testGroup "position transforms"
  [ testProperty "can reverse an applied transform" $ \(transform :: Transform, position :: Position) ->
      revertTransformPos transform (applyTransformPos transform position) == position

  , testProperty "preserves target across transform" $ \(transform :: Transform, board :: Board, position :: Position) ->
      let transformedBoard = applyTransform transform board
          transformedPos = applyTransformPos transform position
          someSign = readBoard board position
          otherSign = readBoard transformedBoard transformedPos

       in someSign == otherSign

  , testProperty "can lookup in old board" $ \(transform :: Transform, board :: Board, position :: Position) ->
      let transformedBoard = applyTransform transform board
          someSign = readBoard transformedBoard position
          transformedPos = revertTransformPos transform position
          otherSign = readBoard board transformedPos

       in someSign == otherSign
  ]


boardKeyTests :: TestTree
boardKeyTests = testGroup "board key"
  [ testProperty "all transforms return the same board key" $ \(transform :: Transform) ->
      let board = applyTransform transform basicBoard
          (key, _) = bestBoardKey board
       in key == "  OXXOOXX"

  ]

  where
    basicBoard = createBoard [Empty, Empty, O, X, X, O, O, X, X]
