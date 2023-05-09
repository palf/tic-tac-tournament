{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Position where

import           GHC.Generics
import           Data.Text       (Text)


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

