module Utils where

import           Board
import           Data.Foldable

setBoard :: [(Position, Sign)] -> Board
setBoard = foldl' setPosition emptyBoard
  where
    setPosition board (pos, sign) = makeMove board sign pos


