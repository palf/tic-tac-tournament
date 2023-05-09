{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Board
  ( Board
  , BoardKey
  , Move
  , Position (..)
  , Sign (..)
  , boardIsFull
  , emptyBoard
  , findWinner
  , getBoardKey
  , getValidMoves
  , identifyPosSource
  , identifyPosTarget
  , makeMove
  , nextSign
  , positionToText
  , renderBoard
    -- for tests
  , Transform (..)
  , applyTransform
  , createBoard
  , readBoard
  , revertTransform
  ) where

import qualified Data.List       as List
import qualified Data.List.Index as Index

import           Data.Text       (Text)
import           GHC.Generics
import           Position


data Sign = Empty | O | X deriving (Bounded, Enum, Eq, Generic, Ord, Show)


signToString :: Sign -> String
signToString Empty = " "
signToString X     = "X"
signToString O     = "O"


signToText :: Sign -> Text
signToText Empty = " "
signToText X     = "X"
signToText O     = "O"


nextSign :: Sign -> Sign
nextSign X     = O
nextSign O     = X
nextSign Empty = X


type Move = Maybe Position


newtype Board
  = Board [Sign]
  deriving (Eq, Generic)


unboard :: Board -> [Sign]
unboard (Board xs) = xs


instance Show Board where
  show = boardToString


readBoard :: Board -> Position -> Sign
readBoard board pos = unboard board !! indexOf pos


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
  fmap (readBoard board . mapPos) allPositions


boardToString :: Board -> String
boardToString (Board xs) = concatMap signToString xs



type BoardKey = Text


boardToText :: Board -> BoardKey
boardToText (Board xs) = foldMap signToText xs


toLists :: Board -> [[Sign]]
toLists board = (fmap . fmap) (readBoard board)
  [ [A1, A2, A3]
  , [B1, B2, B3]
  , [C1, C2, C3]
  ]


renderBoard :: Board -> String
renderBoard = unlines . fmap showRow . toLists
  where
    showRow = List.intercalate "|" . fmap signToString


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
     else error "bad make move"


applyTransform :: Transform -> Board -> Board
applyTransform t = mapBoard (identifyPosSource t)


revertTransform :: Transform -> Board -> Board
revertTransform t = mapBoard (identifyPosTarget t)


getBoardKey :: Board -> (BoardKey, Transform)
getBoardKey board = head $ List.sortOn fst $ List.sortOn snd (permutations board)
  where
    permutations = getBoardKeys


getBoardKeys :: Board -> [(BoardKey, Transform)]
getBoardKeys board = (\x -> (prep x, x)) <$> transforms
  where
    transforms = [ None, Rot1 , Rot2 , Rot3 , Flip , FlipRot1 , FlipRot2 , FlipRot3 ]

    prep x = boardToText (applyTransform x board)



-- "abcdefghi"
-- "ihgfedcba"

-- "adgbehcfi"
-- "ifchebgde"

-- "cbafedihg"
-- "ghidefabc"

-- "cfibehadg"
-- "gdahebifc"
