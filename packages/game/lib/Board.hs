{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Board
  ( Board
  , BoardKey
  , Move
  , Position (..)
  , Sign (..)
  , boardIsFull
  , findWinner
  , getBoardKey
  , getValidMoves
  , identifyPosSource
  , identifyPosTarget
  , initialBoard
  , makeMove
  , nextSign
  , positionToText
  , renderBoard
  ) where

import qualified Data.List    as List
import qualified Data.Text    as Text

import           Data.Text    (Text)
import           GHC.Generics


data Sign = Empty | O | X deriving (Bounded, Enum, Eq, Generic, Ord, Show)


signToString :: Sign -> String
signToString Empty = " "
signToString X     = "X"
signToString O     = "O"


signToText :: Sign -> Text
signToText Empty = " "
signToText X     = "X"
signToText O     = "O"


signFromString :: String -> Maybe Sign
signFromString " " = Just Empty
signFromString "X" = Just X
signFromString "O" = Just O
signFromString _   = Nothing


nextSign :: Sign -> Sign
nextSign X     = O
nextSign O     = X
nextSign Empty = X


data Position = A1 | A2 | A3 | B1 | B2 | B3 | C1 | C2 | C3 deriving
  ( Bounded
  , Enum
  , Eq
  , Generic
  , Ord
  , Show
  )


allPositions :: [Position]
allPositions = [ A1 , A2 , A3 , B1 , B2 , B3 , C1 , C2 , C3 ]


positionToText :: Position -> Text
positionToText A1 = "A1"
positionToText A2 = "A2"
positionToText A3 = "A3"
positionToText B1 = "B1"
positionToText B2 = "B2"
positionToText B3 = "B3"
positionToText C1 = "C1"
positionToText C2 = "C2"
positionToText C3 = "C3"



type Move = Maybe Position


newtype Board
  = Board [Sign]
  deriving (Eq, Generic)


unboard :: Board -> [Sign]
unboard (Board xs) = xs


instance Show Board where
  show = boardToString


indexOf :: Position -> Int
indexOf A1 = 0
indexOf A2 = 1
indexOf A3 = 2
indexOf B1 = 3
indexOf B2 = 4
indexOf B3 = 5
indexOf C1 = 6
indexOf C2 = 7
indexOf C3 = 8


readBoard :: Board -> Position -> Sign
readBoard board pos = unboard board !! indexOf pos


writeBoard :: Board -> Sign -> Position -> Board
writeBoard board sign pos = Board ys
  where
    ys = before <> pure sign <> List.drop 1 after
    (before, after) = List.splitAt (indexOf pos) (unboard board)


createBoard ::  [Sign] -> Board
createBoard xs
  | length xs == 9 = Board xs
  | otherwise      = error "bad input for board"


initialBoard :: Board
initialBoard = createBoard (List.replicate 9 Empty)


mapBoard :: (Position -> Position) -> Board -> Board
mapBoard mapPos board = Board $
  fmap (readBoard board . mapPos) allPositions


boardToString :: Board -> String
boardToString (Board xs) = concatMap signToString xs



type BoardKey = Text


boardToText :: Board -> BoardKey
boardToText (Board xs) = foldMap signToText xs


boardFromString :: String -> Maybe Board
boardFromString str
  | length str == 9
   = createBoard <$> mapM (\c -> signFromString [c]) str

  | otherwise = Nothing


toList :: Board -> [Sign]
toList (Board xs) = xs


toLists :: Board -> [[Sign]]
toLists board = (fmap . fmap) (readBoard board )
  [ [A1, A2, A3]
  , [B1, B2, B3]
  , [C1, C2, C3]
  ]


renderBoard :: Board -> String
renderBoard = unlines . fmap showRow . toLists
  where
    showRow = List.intercalate "|" . fmap signToString


boardIsFull :: Board -> Bool
boardIsFull = not . any isEmpty . toList


isEmpty :: Sign -> Bool
isEmpty Empty = True
isEmpty _     = False


findWinner :: Board -> Maybe Sign
findWinner (Board xs)
  | isNotEmpty a1Value && a1Value == a2Value && a1Value == a3Value = Just a1Value
  | isNotEmpty a1Value && a1Value == b1Value && a1Value == c1Value = Just a1Value
  | isNotEmpty a1Value && a1Value == b2Value && a1Value == c3Value = Just a1Value

  | isNotEmpty a2Value && a2Value == b2Value && a2Value == c2Value = Just a2Value
  | isNotEmpty b1Value && b1Value == b2Value && b1Value == b3Value = Just b1Value

  | isNotEmpty a3Value && a3Value == b2Value && a3Value == c1Value = Just a3Value
  | isNotEmpty a3Value && a3Value == b3Value && a3Value == c3Value = Just a3Value

  | isNotEmpty c1Value && c1Value == c2Value && c1Value == c3Value = Just c1Value

  | otherwise = Nothing

  where
    isNotEmpty = not . isEmpty

    a1Value = head xs
    a2Value = xs !! 1
    a3Value = xs !! 2
    b1Value = xs !! 3
    b2Value = xs !! 4
    b3Value = xs !! 5
    c1Value = xs !! 6
    c2Value = xs !! 7
    c3Value = xs !! 8


getValidMoves :: Board -> [Position]
getValidMoves board = filter isValidMove allPositions
  where
    isValidMove :: Position -> Bool
    isValidMove = isEmpty . readBoard board


makeMove :: Board -> Sign -> Position -> Board
makeMove board sign pos =
  if readBoard board pos == Empty
     then writeBoard board sign pos
     else error "bad make move"


data Transform = None | Flip | Rot1 | FlipRot1 | Rot2 | FlipRot2 | Rot3 | FlipRot3 deriving
  ( Bounded
  , Enum
  , Eq
  , Generic
  , Ord
  , Show
  )


-- read this as position at A1 comes from C1
rotatePos :: Position -> Position
rotatePos A1 = C1
rotatePos A2 = B1
rotatePos A3 = A1
rotatePos B1 = C2
rotatePos B2 = B2
rotatePos B3 = A2
rotatePos C1 = C3
rotatePos C2 = B3
rotatePos C3 = A3


flipPos :: Position -> Position
flipPos A1 = A3
flipPos A2 = A2
flipPos A3 = A1
flipPos B1 = B3
flipPos B2 = B2
flipPos B3 = B1
flipPos C1 = C3
flipPos C2 = C2
flipPos C3 = C1


identifyPosSource :: Transform -> Position -> Position
identifyPosSource None     = id
identifyPosSource Rot1     = rotatePos
identifyPosSource Rot2     = rotatePos . rotatePos
identifyPosSource Rot3     = rotatePos . rotatePos  . rotatePos
identifyPosSource Flip     = flipPos
identifyPosSource FlipRot1 = rotatePos . flipPos
identifyPosSource FlipRot2 = rotatePos . rotatePos . flipPos
identifyPosSource FlipRot3 = rotatePos . rotatePos . rotatePos . flipPos


identifyPosTarget :: Transform -> Position -> Position
identifyPosTarget None     = id
identifyPosTarget Rot1     = identifyPosSource Rot3
identifyPosTarget Rot2     = identifyPosSource Rot2
identifyPosTarget Rot3     = identifyPosSource Rot1
identifyPosTarget Flip     = identifyPosSource Flip
identifyPosTarget FlipRot1 = flipPos . rotatePos . rotatePos . rotatePos
identifyPosTarget FlipRot2 = flipPos . rotatePos . rotatePos
identifyPosTarget FlipRot3 = flipPos . rotatePos


applyTransform :: Transform -> Board -> Board
applyTransform t = mapBoard (identifyPosSource t)


revertTransform :: Transform -> Board -> Board
revertTransform None     = id
revertTransform Rot1     = applyTransform Rot3
revertTransform Rot2     = applyTransform Rot2
revertTransform Rot3     = applyTransform Rot1
revertTransform Flip     = applyTransform Flip
revertTransform FlipRot1 = mapBoard (flipPos . rotatePos . rotatePos . rotatePos)
revertTransform FlipRot2 = mapBoard (flipPos . rotatePos . rotatePos )
revertTransform FlipRot3 = mapBoard (flipPos . rotatePos )


getBoardKey :: Board -> (BoardKey, Transform)
getBoardKey board = head $ List.sortOn fst $ List.sortOn snd (permutations board)
  where
    permutations :: Board -> [] ( BoardKey, Transform )
    permutations b = (\x -> (boardToText (applyTransform x b), x)) <$>
      [ None, Rot1 , Rot2 , Rot3 , Flip , FlipRot1 , FlipRot2 , FlipRot3 ]
