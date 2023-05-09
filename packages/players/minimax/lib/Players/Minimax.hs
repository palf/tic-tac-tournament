module Players.Minimax
  ( Score (..)
  , assess
  , optimiseFor
  ) where

import qualified Data.List                  as List
import qualified Data.Maybe                 as Maybe

import           Board
import           Control.Monad.Random.Class (MonadRandom)
import           Data.Functor               ((<&>))
import           System.Random.Shuffle      (shuffleM)


data Score = WinForO | Draw | WinForX deriving (Enum, Eq, Ord, Show)


optimiseFor :: (MonadRandom m) => Sign -> Board -> m (Maybe Position)
optimiseFor sign board = do
  Maybe.listToMaybe <$> assessOptions

  where
    assessOptions :: (MonadRandom m) => m [Position]
    assessOptions = do
      shuffleM (assess sign board) <&> (fmap fst . List.sortBy sortOrder)

    sortOrder :: (a, Score) -> (a, Score) -> Ordering
    sortOrder (_, a) (_, b) = if sign == X then compare b a else compare a b


assess :: Sign -> Board -> [(Position, Score)]
assess sign board
  = zip moves (checkScoreAfterMove board sign <$> moves)

  where
    moves = getValidMoves board


getScoreForBoard :: Sign -> Board -> Score
getScoreForBoard sign b
  | Just winner <- findWinner b
    = if winner == X then WinForX else WinForO

  | List.null candidates
    = Draw

  | otherwise
    = (if sign == X then maximum else minimum)
        (checkScoreAfterMove b sign <$> candidates)

  where
    candidates = getValidMoves b


checkScoreAfterMove :: Board -> Sign -> Position -> Score
checkScoreAfterMove board sign
  = getScoreForBoard (nextSign sign) . makeMove board sign
