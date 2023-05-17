{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module TicTacTournament.Sign where

import           Data.ByteString (ByteString)
import           Data.Text       (Text)
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


signToByteString :: Sign -> ByteString
signToByteString Empty = " "
signToByteString X     = "X"
signToByteString O     = "O"


nextSign :: Sign -> Sign
nextSign X     = O
nextSign O     = X
nextSign Empty = X
