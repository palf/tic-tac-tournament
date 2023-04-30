{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Main
  ( main
  , run
  , runMany
  , withWeightsFile
  ) where

import qualified Control.Monad.State       as State
import qualified Data.Map.Strict           as Map
import qualified Players.Learner           as Learner
import qualified Players.Learner.Weights   as Weights
import qualified Players.Perfect           as Perfect

import           Board
import           Control.Monad
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.State       (StateT, execStateT, runStateT)
import           Control.Monad.Trans.Class
import           Control.Monad.Writer      (MonadWriter, WriterT, runWriterT)
import           Play
import           Players.Learner.Weights   (HasWeights, Weights)



instance (Monad m) => HasWeights (StateT Weights m) where
  readWeights = State.get
  writeWeights = State.put


newtype App m a
  = App { unApp :: WriterT GameHistory (WriterT [Weights.Choice] (StateT Weights m)) a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadWriter GameHistory)

instance (Monad m) => HasWeights (App m) where
  readWeights = App $ lift $ lift State.get
  writeWeights = App . lift . lift . State.put

instance (Monad m) => Learner.RecordsChoice (App m) where
  recordChoice = App . lift . Learner.recordChoice


run :: forall m. (MonadIO m) => Int -> Weights -> m Weights
run n ws = do
  when (mod n 1000 == 0) $ Weights.writeWeightsToFile ws
  execStateT op ws

  where
    op = do
      let (xPlayer :: Player (App m)) = Player (Learner.optimiseFor X)
      let oPlayer = Player $ Perfect.bestPlay O
      let players = Map.fromList [(X, xPlayer), (O, oPlayer)]

      ((end, game), choices) <- runWriterT (runWriterT ( unApp $ play1 players ))

      liftIO $ printGame game

      liftIO $ putStrLn $ case end of
        Winner x   -> show x <> " wins!"
        NoPlayer x -> "no player found for " <> show x
        Draw       -> "Draw"
        NoMoves x  -> "no moves for " <> show x

      case end of
        Winner X -> Weights.increaseWeights choices >> error "x won!?"
        Draw     -> Weights.steadyWeights choices
        _        -> Weights.decreaseWeights choices




runMany :: (MonadIO m) => Int -> Weights -> m Weights
runMany 0 = pure
runMany n = run n >=> runMany (n - 1)


withWeightsFile :: (MonadIO m) => (Weights -> m Weights) -> m ()
withWeightsFile op = do
  weights <- Weights.readWeightsFromFile
  updatedWeights <- op weights
  Weights.writeWeightsToFile updatedWeights


main :: IO ()
main = withWeightsFile ( runMany 1 )
