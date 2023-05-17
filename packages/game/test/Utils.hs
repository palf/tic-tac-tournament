module Utils where

import           Data.Foldable
import           TicTacTournament

setBoard :: [(Position, Sign)] -> Board
setBoard = foldl' setPosition emptyBoard
  where
    setPosition board (pos, sign) = makeMove board sign pos


