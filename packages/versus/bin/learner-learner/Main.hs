{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}


module Main (main) where

import qualified Control.Monad.State       as State
import qualified Data.Map.Strict           as Map
import qualified Players.Learner           as Learner
import qualified Players.Learner.Weights   as Weights

import           Board
import           Control.Monad
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.State       (StateT, execStateT, runStateT)
import           Control.Monad.Trans.Class
import           Control.Monad.Writer      (MonadWriter, WriterT, runWriterT)
import           Play
import           Players.Learner.Weights   (HasWeights, Weights)


newtype App m a
  = App { unApp :: WriterT GameHistory (StateT [Weights.Choice] (StateT [Weights.Choice] (StateT Weights m))) a }
  deriving (Applicative, Functor, Monad, MonadWriter GameHistory)


instance (Monad m) => HasWeights (App m) where
  readWeights = App $ lift $ lift $ lift State.get
  writeWeights = App . lift . lift . lift . State.put


-- TODO: remove these orphans
instance (Monad m) => HasWeights (StateT Weights m) where
  readWeights = State.get
  writeWeights = State.put

instance (Monad m) => HasWeights (StateT [Weights.Choice] (StateT Weights m)) where
  readWeights = lift State.get
  writeWeights = lift . State.put

instance (Monad m) => HasWeights (StateT [Weights.Choice] (StateT [Weights.Choice] (StateT Weights m))) where
  readWeights = lift $ lift State.get
  writeWeights = lift . lift . State.put



run :: (MonadIO m) => Int -> Weights -> m Weights
run n ws = do
  when (mod n 1000 == 0) $ Weights.writeWeightsToFile ws
  execStateT op ws

  where
    op :: forall m. (MonadIO m) => StateT Weights m ()
    op = do
      let (xPlayer :: Player (App m)) = Player $ App . lift . Learner.optimiseFor X
      let (oPlayer :: Player (App m)) = Player $ App . lift . lift . Learner.optimiseFor O
      let players = Map.fromList [(X, xPlayer), (O, oPlayer)]

      let ( letsplay :: App m GameResult ) = play1 players
      (((end, game), choicesX), choicesO) <- runStateT (runStateT (runWriterT $ unApp letsplay) mempty) mempty

      liftIO $ printGame game

      liftIO $ putStrLn $ case end of
        Winner x   -> show x <> " wins!"
        NoPlayer x -> "no player found for " <> show x
        Draw       -> "Draw"
        NoMoves x  -> "no moves for " <> show x

      case end of
        Winner X -> Weights.increaseWeights choicesX >> Weights.increaseWeights choicesO
        Winner O -> Weights.decreaseWeights choicesX >> Weights.decreaseWeights choicesO
        Draw -> Weights.steadyWeights choicesO >> Weights.steadyWeights choicesX
        _ -> pure ()



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
