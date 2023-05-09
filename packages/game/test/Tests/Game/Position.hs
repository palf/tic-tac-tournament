{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tests.Game.Position (positionTests) where

import qualified Data.Aeson            as Aeson
import qualified Data.Map              as Map

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           Board


instance Arbitrary Transform where
  arbitrary = arbitraryBoundedEnum
  shrink None = []
  shrink _    = [None]


-- instance Arbitrary Sign where
--   arbitrary = arbitraryBoundedEnum


instance Arbitrary Position where
  arbitrary = arbitraryBoundedEnum



positionTests :: TestTree
positionTests = testGroup "position"
  [ flipTests
  , rotateTests
  , transformTests
  , jsonTests
  ]


flipTests :: TestTree
flipTests = testGroup "flip"
  [ testCase "does not move the center column" $ do
      let position = A2
      identifyPosSource Flip position @?= position

  , testCase "swaps the left and right columns" $ do
      let position = A1
      let expected = A3
      identifyPosSource Flip position @?= expected
  ]


rotateTests :: TestTree
rotateTests = testGroup "rotate"
  [ testCase "has no effect on center" $ do
      identifyPosSource Rot1 B2 @?= B2

  , testCase "rotates once clockwise" $ do
      identifyPosSource Rot1 A1 @?= C1
  ]


transformTests :: TestTree
transformTests = testGroup "transform"
  [ testProperty "can reverse an applied transform" $ \(transform :: Transform, position :: Position) ->
      (identifyPosTarget transform . identifyPosSource transform) position == position

  , testProperty "can reverse an applied transform" $ \(transform :: Transform, position :: Position) ->
      (identifyPosSource transform . identifyPosTarget transform) position == position
  ]


jsonTests :: TestTree
jsonTests = testGroup "json"
  [ testCase "can convert to json" $ do
      let position = A1
      Aeson.encode position @?= "\"A1\""

  , testCase "can convert from json" $ do
      Aeson.eitherDecode "\"A1\"" @?= Right A1

  , testCase "can convert to json key" $ do
      Aeson.encode (Map.fromList [(A1, 1 :: Int)]) @?= "{\"A1\":1}"

  , testCase "can convert from json key" $ do
      Aeson.eitherDecode "{\"A1\":1}" @?= Right (Map.fromList [(A1, 1 :: Int)])

  , testProperty "can decode an encoded string" $ \(position :: Position) ->
      Aeson.eitherDecode (Aeson.encode position) == Right position
  ]
