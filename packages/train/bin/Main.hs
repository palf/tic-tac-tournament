{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Main (main) where

import qualified Control.Monad.State.Strict as State
import qualified Data.Map.Strict            as Map
import qualified Players.Learner.File       as File
import qualified Players.Learner02          as Learner02

import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Random.Class (MonadRandom)
import           Control.Monad.State.Strict (MonadState, StateT, evalStateT,
                                             runStateT)
import           Control.Monad.Trans.Class  (lift)
import           System.ProgressBar

import           Board
import           Options
import           Play


type Weights = Learner02.Weights'


newtype App m a
  = App { unApp :: StateT GameHistory (StateT [Learner02.Choice'] (StateT Weights m)) a }
  deriving
  ( Applicative
  , Functor
  , Monad
  , MonadIO
  , MonadRandom
  , MonadState GameHistory
  )

type SomePlayer = Player (App IO)


instance (Monad m) => Learner02.HasWeights' (App m) where
  readw = App $ lift $ lift $ State.gets id
  writew ws = App $ lift $ lift $ do State.put ws

instance (Monad m) => Learner02.RecordsChoice' (App m) where
  recordChoice' = App . lift . Learner02.recordChoice'


runApp :: (Monad m) => App m a -> StateT Weights m (a, GameHistory, [Learner02.Choice'])
runApp op = do
  ((a, b), c) <- runStateT (runStateT ( unApp op) mempty) mempty
  pure (a, b, c)


run :: (MonadIO m) => App m GameResult -> Weights -> m Weights
run game ws = do
  -- when (mod n 1000 == 0) $ Weights.writeWeightsToFile ws

  ((gameResult, gameHistory, choices), updatedWeights) <- runStateT (runApp game) ws

  uw2 <- evalStateT (case gameResult of
    Winner X -> Learner02.recordXWin choices
    Winner O -> Learner02.recordOWin choices
    Draw     -> Learner02.recordDraw choices
    _        -> pure mempty
            ) updatedWeights

  -- liftIO $ do
  --    printGame gameHistory

  --    putStrLn $ case gameResult of
  --      Winner x   -> show x <> " wins!"
  --      NoPlayer x -> "no player found for " <> show x
  --      Draw       -> "Draw"
  --      NoMoves x  -> "no moves for " <> show x

  --    putStrLn ""

  pure uw2


runMany :: forall m. (MonadIO m) => App m GameResult -> Int -> Weights -> m Weights
runMany game n weights = do
  pb <- liftIO $ createBar n
  f pb n weights

  where
    createBar x = newProgressBar defStyle 10 (Progress 0 x ())

    f :: (MonadIO m) => ProgressBar s -> Int -> Weights -> m Weights
    f _ 0 ws = pure ws
    f pb x ws = do
      ws' <- run game ws
      liftIO $ incProgress pb 1
      f pb (x - 1) ws'


learner02Player :: (MonadIO m) => Sign -> Player (App m)
learner02Player = Player . Learner02.adjustRisk 0.92


execute :: (SomePlayer, SomePlayer) -> Weights -> Int -> IO Weights
execute (xPlayer, oPlayer) inputWeights n = do
  let players = Map.fromList [(X, xPlayer), (O, oPlayer)]
  let game = play players
  runMany game n inputWeights


main :: IO ()
main = do
  options <- readOptions

  let xPlayer = learner02Player X
  let oPlayer = learner02Player O

  inputWeights <- Learner02.Weights' <$> File.readWeightsFromFile

  (Learner02.Weights' ws) <- execute (xPlayer, oPlayer) inputWeights (total options)

  File.writeWeightsToFile ws
