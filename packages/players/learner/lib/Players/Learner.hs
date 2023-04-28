{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Players.Learner where

import qualified Control.Monad.Random       as Random
import qualified Control.Monad.State        as State
import qualified Control.Monad.Writer       as Writer
import qualified Data.List                  as List

import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.State        (MonadState, StateT)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Writer       (MonadWriter, WriterT)
import           Safe                       (headMay, lastMay)

import           Board
import           Players.Learner.Weights


data Action = Exploit | Explore deriving (Eq, Show)


randomElementIO :: [a] -> IO (Maybe a)
randomElementIO [] = pure Nothing
randomElementIO xs = do
  idx <- Random.randomRIO (0, Prelude.length xs - 1)
  pure $ Just (xs !! idx)


pickAny :: (MonadIO m) => Board -> m Move
pickAny = liftIO . randomElementIO . getValidMoves


pickAction :: (MonadIO m) => Rational -> m Action
pickAction n
  | n < 0 = error "invalid n < 0"
  | n > 1 = error "invalid n > 1"
  | otherwise = liftIO $ Random.weighted [(Exploit, n), (Explore, 1 - n)]



optimiseFor :: (MonadIO m, HasWeights m) => Sign -> Board -> StateT [Choice] m Move
optimiseFor sign board = do
  action <- pickAction 0.8

  case action of
    Explore -> do
      move <- pickAny board
      recordExploration board move
      pure move

    Exploit -> do
      weights <- lift readWeights
      let validMoves = getValidMoves board
      let weightedMoves = zip validMoves $ getWeight weights board <$> validMoves
      let moves = List.sortOn snd weightedMoves
      let selection = case sign of
               X -> fst <$> lastMay moves
               O -> fst <$> headMay moves
               _ -> Nothing

      liftIO $ print (sign, selection, moves)

      pure selection


type ChoiceRecorder m = MonadState [Choice] m


recordExploration :: (MonadIO m, ChoiceRecorder m) => Board -> Move -> m ()
recordExploration board move = do
  choices <- State.get
  State.put $ choices <> [(board, move)]
