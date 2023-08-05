{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module TicTacTournament.Board
  ( Board
  , BoardKey (..)
  , Move
  , boardIsFull
  , emptyBoard
  , findWinner
  , getBoardKey
  , getValidMoves
  , makeMove
  , nextSign
  , positionToText
  , renderBoard
    -- for tests
  , Transform (..)
  , applyTransform
  , boardToByteString
  , boardToText
  , createBoard
  , readBoard
  , revertTransform
  ) where

import qualified Data.Aeson                as Aeson
import qualified Data.List                 as List
import qualified Data.List.Index           as Index

import           Data.ByteString           (ByteString)
import           Data.Text                 (Text)
import           GHC.Generics
import           TicTacTournament.Position
import           TicTacTournament.Sign



type Move = Maybe Position


newtype Board
  = Board [Sign]
  deriving (Eq, Generic)


boardToString :: Board -> String
boardToString (Board xs) = concatMap signToString xs


instance Show Board where
  show = boardToString


boardToByteString :: Board -> ByteString
boardToByteString (Board xs)
    = row1 <> "|" <> row2 <> "|" <> row3

  where
    ts = signToByteString <$> xs
    row1 = head ts <> ts !! 1 <> ts !! 2
    row2 = ts !! 3 <> ts !! 4 <> ts !! 5
    row3 = ts !! 6 <> ts !! 7 <> ts !! 8


boardToText :: Board -> Text
boardToText (Board xs)
    = row1 <> "|" <> row2 <> "|" <> row3

  where
    ts = signToText <$> xs
    row1 = head ts <> ts !! 1 <> ts !! 2
    row2 = ts !! 3 <> ts !! 4 <> ts !! 5
    row3 = ts !! 6 <> ts !! 7 <> ts !! 8


renderBoard :: Board -> String
renderBoard board = unlines $ showRow . fmap (readBoard board) <$> ps
  where
    showRow :: [Sign] -> String
    showRow = List.intercalate "|" . fmap signToString

    ps =
      [ [A1, A2, A3]
      , [B1, B2, B3]
      , [C1, C2, C3]
      ]


readBoard :: Board -> Position -> Sign
readBoard (Board xs) pos = xs !! indexOf pos


writeBoard :: Board -> Sign -> Position -> Board
writeBoard (Board xs) sign pos = Board (Index.setAt (indexOf pos) sign xs)


createBoard ::  [Sign] -> Board
createBoard xs
  | length xs == 9 = Board xs
  | otherwise      = error "bad input for board"


emptyBoard :: Board
emptyBoard = createBoard (List.replicate 9 Empty)


mapBoard :: (Position -> Position) -> Board -> Board
mapBoard mapPos board = Board $
  readBoard board . mapPos <$> allPositions


boardIsFull :: Board -> Bool
boardIsFull (Board xs) = Empty `notElem` xs


findWinner :: Board -> Maybe Sign
findWinner (Board [a1, a2, a3, b1, b2, b3, c1, c2, c3])
  | b2 /= Empty && b2 == b1 && b2 == b3 = Just b2
  | b2 /= Empty && b2 == a2 && b2 == c2 = Just b2

  | b2 /= Empty && b2 == a1 && b2 == c3 = Just b2
  | b2 /= Empty && b2 == a3 && b2 == c1 = Just b2

  | a1 /= Empty && a1 == a2 && a1 == a3 = Just a1
  | a1 /= Empty && a1 == b1 && a1 == c1 = Just a1

  | c3 /= Empty && c3 == b3 && c3 == a3 = Just c3
  | c3 /= Empty && c3 == c1 && c3 == c2 = Just c3

  | otherwise = Nothing

findWinner (Board _) = Nothing


getValidMoves :: Board -> [Position]
getValidMoves (Board xs) = [ pos | (sign, pos) <- zip xs allPositions, sign == Empty ]


makeMove :: Board -> Sign -> Position -> Board
makeMove board sign pos =
  if board `readBoard` pos == Empty
     then writeBoard board sign pos
     else error "attempted to set non-empty position"


applyTransform :: Transform -> Board -> Board
applyTransform t = mapBoard (identifyPosSource t)


revertTransform :: Transform -> Board -> Board
revertTransform t = mapBoard (identifyPosTarget t)


newtype BoardKey
  = BoardKey Text
  deriving (Aeson.FromJSONKey, Aeson.ToJSONKey, Eq, Generic, Ord, Show)


getBoardKey :: Board -> (BoardKey, Transform)
getBoardKey board = head $ List.sortOn fst $ List.sortOn snd (getBoardKeys board)


getBoardKeys :: Board -> [(BoardKey, Transform)]
getBoardKeys board = (\x -> (prep x, x)) <$> transforms
  where
    transforms = [ None, Rot1 , Rot2 , Rot3 , Flip , FlipRot1 , FlipRot2 , FlipRot3 ]

    prep x = BoardKey $ boardToText (applyTransform x board)



-- "abcdefghi"
-- "ihgfedcba"

-- "adgbehcfi"
-- "ifchebgde"

-- "cbafedihg"
-- "ghidefabc"

-- "cfibehadg"
-- "gdahebifc"
