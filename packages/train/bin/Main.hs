{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Main (main) where

import qualified Control.Monad.State.Strict as State
import qualified Data.Map.Strict            as Map
import qualified Players.Learner.File       as File
import qualified Players.Learner02          as Learner02

import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Random.Class (MonadRandom)
import           Control.Monad.Trans.Class (MonadTrans)
import           Control.Monad.State.Strict (MonadState, StateT, evalStateT,
                                             runStateT)
import           Control.Monad.Trans.Class  (lift)
import           System.ProgressBar

import           Board
import           Options
import           Play


type Weights = Learner02.Weights'
type Choice = Learner02.Choice'


newtype App m a
  = App { unApp :: (StateT [Choice] (StateT Weights m)) a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadRandom)

instance (Monad m) => MonadState GameHistory (App m) where
  get = pure mempty
  put _ = pure ()



instance (Monad m) => Learner02.HasWeights' (App m) where
  readw = App $ lift $ State.gets id
  writew ws = App $ lift $ do State.put ws

instance (Monad m) => Learner02.RecordsChoice' (App m) where
  recordChoice' = App . Learner02.recordChoice'


runApp :: (Monad m) => App m a -> StateT Weights m (a, [Choice])
runApp op = do
  (a, c) <- runStateT ( unApp op) mempty
  pure (a, c)

run :: (MonadIO m) => App m GameResult -> Weights -> m Weights
run game ws = do
  ((gameResult, choices), updatedWeights) <- runStateT (runApp game) ws

  evalStateT (case gameResult of
    Winner X -> Learner02.recordXWin choices
    Winner O -> Learner02.recordOWin choices
    Draw     -> Learner02.recordDraw choices
    _        -> pure mempty
            ) updatedWeights


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


type SomePlayer = Player (App IO)


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
