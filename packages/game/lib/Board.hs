
{-# LANGUAGE DeriveGeneric #-}

module Board where

import qualified Data.Array     as Array
import qualified Data.List      as List

import           Data.Array     (Array, (!), (//))
import           Data.Bifunctor
import           GHC.Generics


data Sign = Empty | O | X deriving (Bounded, Enum, Eq, Generic, Ord, Show)


signToString :: Sign -> String
signToString Empty = " "
signToString X     = "X"
signToString O     = "O"


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


positionToString :: Position -> String
positionToString = show


positionFromString :: String -> Maybe Position
positionFromString "A1" = Just A1
positionFromString "A2" = Just A2
positionFromString "A3" = Just A3
positionFromString "B1" = Just B1
positionFromString "B2" = Just B2
positionFromString "B3" = Just B3
positionFromString "C1" = Just C1
positionFromString "C2" = Just C2
positionFromString "C3" = Just C3
positionFromString _    = Nothing


type Move = Maybe Position


data Board
  = Board
    { a1 :: Sign
    , a2 :: Sign
    , a3 :: Sign
    , b1 :: Sign
    , b2 :: Sign
    , b3 :: Sign
    , c1 :: Sign
    , c2 :: Sign
    , c3 :: Sign
    }
  deriving (Eq, Generic)

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
readBoard board A1 = a1 board
readBoard board A2 = a2 board
readBoard board A3 = a3 board
readBoard board B1 = b1 board
readBoard board B2 = b2 board
readBoard board B3 = b3 board
readBoard board C1 = c1 board
readBoard board C2 = c2 board
readBoard board C3 = c3 board


writeBoard :: Board -> Sign -> Position -> Board
writeBoard board sign A1 = board { a1 = sign }
writeBoard board sign A2 = board { a2 = sign }
writeBoard board sign A3 = board { a3 = sign }
writeBoard board sign B1 = board { b1 = sign }
writeBoard board sign B2 = board { b2 = sign }
writeBoard board sign B3 = board { b3 = sign }
writeBoard board sign C1 = board { c1 = sign }
writeBoard board sign C2 = board { c2 = sign }
writeBoard board sign C3 = board { c3 = sign }


createBoard ::  [Sign] -> Board
createBoard xs
  | length xs == 9 = Board
    { a1 = head xs
    , a2 = xs !! 1
    , a3 = xs !! 2
    , b1 = xs !! 3
    , b2 = xs !! 4
    , b3 = xs !! 5
    , c1 = xs !! 6
    , c2 = xs !! 7
    , c3 = xs !! 8
    }
  | otherwise     = error "bad input for board"


initialBoard :: Board
initialBoard = createBoard (List.replicate 9 Empty)


mapBoard :: (Position -> Position) -> Board -> Board
mapBoard mapPos board = createBoard $
  fmap (readBoard board . mapPos) allPositions


-- TODO: change board to a matrix
rotateBoard :: Board -> Board
rotateBoard = mapBoard f
  where
    f A1 = C1
    f A2 = B1
    f A3 = A1
    f B1 = C2
    f B2 = B2
    f B3 = A2
    f C1 = C3
    f C2 = B3
    f C3 = A3


flipBoard :: Board -> Board
flipBoard = mapBoard f
  where
    f A1 = A3
    f A2 = A2
    f A3 = A1
    f B1 = B3
    f B2 = B2
    f B3 = B1
    f C1 = C3
    f C2 = C2
    f C3 = C1



boardToString :: Board -> String
boardToString board = concatMap signToString arr
  where
    arr = fmap (readBoard board) allPositions


boardFromString :: String -> Maybe Board
boardFromString str
  | length str == 9
   = createBoard <$> mapM (\c -> signFromString [c]) str

  | otherwise = Nothing



toList :: Board -> [Sign]
toList board = readBoard board <$> allPositions


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
findWinner board
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

    a1Value = readBoard board A1
    a2Value = readBoard board A2
    a3Value = readBoard board A3
    b1Value = readBoard board B1
    b2Value = readBoard board B2
    b3Value = readBoard board B3
    c1Value = readBoard board C1
    c2Value = readBoard board C2
    c3Value = readBoard board C3


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


applyTransformPos :: Transform -> Position -> Position
applyTransformPos None p      = p
applyTransformPos Rot1 A1     = A3
applyTransformPos Rot1 A2     = B3
applyTransformPos Rot1 A3     = C3
applyTransformPos Rot1 B1     = A2
applyTransformPos Rot1 B2     = B2
applyTransformPos Rot1 B3     = C2
applyTransformPos Rot1 C1     = A1
applyTransformPos Rot1 C2     = B1
applyTransformPos Rot1 C3     = C1
applyTransformPos Rot2 A1     = C3
applyTransformPos Rot2 A2     = C2
applyTransformPos Rot2 A3     = C1
applyTransformPos Rot2 B1     = B3
applyTransformPos Rot2 B2     = B2
applyTransformPos Rot2 B3     = B1
applyTransformPos Rot2 C1     = A3
applyTransformPos Rot2 C2     = A2
applyTransformPos Rot2 C3     = A1
applyTransformPos Rot3 A1     = C1
applyTransformPos Rot3 A2     = B1
applyTransformPos Rot3 A3     = A1
applyTransformPos Rot3 B1     = C2
applyTransformPos Rot3 B2     = B2
applyTransformPos Rot3 B3     = A2
applyTransformPos Rot3 C1     = C3
applyTransformPos Rot3 C2     = B3
applyTransformPos Rot3 C3     = A3
applyTransformPos Flip A1     = A3
applyTransformPos Flip A2     = A2
applyTransformPos Flip A3     = A1
applyTransformPos Flip B1     = B3
applyTransformPos Flip B2     = B2
applyTransformPos Flip B3     = B1
applyTransformPos Flip C1     = C3
applyTransformPos Flip C2     = C2
applyTransformPos Flip C3     = C1
applyTransformPos FlipRot1 A1 = A1
applyTransformPos FlipRot1 A2 = B1
applyTransformPos FlipRot1 A3 = C1
applyTransformPos FlipRot1 B1 = A2
applyTransformPos FlipRot1 B2 = B2
applyTransformPos FlipRot1 B3 = C2
applyTransformPos FlipRot1 C1 = A3
applyTransformPos FlipRot1 C2 = B3
applyTransformPos FlipRot1 C3 = C3
applyTransformPos FlipRot2 A1 = C1
applyTransformPos FlipRot2 A2 = C2
applyTransformPos FlipRot2 A3 = C3
applyTransformPos FlipRot2 B1 = B1
applyTransformPos FlipRot2 B2 = B2
applyTransformPos FlipRot2 B3 = B3
applyTransformPos FlipRot2 C1 = A1
applyTransformPos FlipRot2 C2 = A2
applyTransformPos FlipRot2 C3 = A3
applyTransformPos FlipRot3 A1 = C3
applyTransformPos FlipRot3 A2 = B3
applyTransformPos FlipRot3 A3 = A3
applyTransformPos FlipRot3 B1 = C2
applyTransformPos FlipRot3 B2 = B2
applyTransformPos FlipRot3 B3 = A2
applyTransformPos FlipRot3 C1 = C1
applyTransformPos FlipRot3 C2 = B1
applyTransformPos FlipRot3 C3 = A1



revertTransformPos :: Transform -> Position -> Position
revertTransformPos None     = id
revertTransformPos Rot1     = applyTransformPos Rot3
revertTransformPos Rot2     = applyTransformPos Rot2
revertTransformPos Rot3     = applyTransformPos Rot1
revertTransformPos Flip     = applyTransformPos Flip
revertTransformPos FlipRot1 = applyTransformPos FlipRot1
revertTransformPos FlipRot2 = applyTransformPos FlipRot2
revertTransformPos FlipRot3 = applyTransformPos FlipRot3




applyTransform :: Transform -> Board -> Board
applyTransform None b = b
applyTransform Rot1 b = rotateBoard b
applyTransform Rot2 b = rotateBoard $ rotateBoard b
applyTransform Rot3 b = rotateBoard $ rotateBoard $ rotateBoard b
applyTransform Flip b = flipBoard b
applyTransform FlipRot1 b = flipBoard $ rotateBoard b
applyTransform FlipRot2 b = flipBoard $ rotateBoard $ rotateBoard b
applyTransform FlipRot3 b = flipBoard $ rotateBoard $ rotateBoard $ rotateBoard b


revertTransform :: Transform -> Board -> Board
revertTransform None b = b
revertTransform Rot1 b = rotateBoard $ rotateBoard $ rotateBoard b
revertTransform Rot2 b = rotateBoard $ rotateBoard b
revertTransform Rot3 b = rotateBoard b
revertTransform Flip b = flipBoard b
revertTransform FlipRot1 b = rotateBoard $ rotateBoard $ rotateBoard $ flipBoard b
revertTransform FlipRot2 b = rotateBoard $ rotateBoard $ flipBoard b
revertTransform FlipRot3 b = rotateBoard $ flipBoard b


bestRotation :: Board -> (String, Transform)
bestRotation board = head $ List.sortOn fst $ List.sortOn snd (first boardToString <$> permutations board)
  where
    permutations :: Board -> [(Board, Transform)]
    permutations b = fmap (\x -> (applyTransform x b, x))
      [ None , Rot1 , Rot2 , Rot3 , Flip , FlipRot1 , FlipRot2 , FlipRot3 ]


bestBoardKey :: Board -> (String, Transform)
bestBoardKey = bestRotation
