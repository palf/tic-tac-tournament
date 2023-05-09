{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Main (main) where

import qualified Control.Monad.State.Strict as State
import qualified Data.Aeson                 as Aeson
import qualified Data.Map.Strict            as Map
import qualified Data.Maybe                 as Maybe
import qualified Players.Learner.File       as File
import qualified Players.Learner01          as Learner01
import qualified Players.Learner02          as Learner02
import qualified Players.Minimax            as Minimax
import qualified Players.Perfect            as Perfect
import qualified Players.Random             as Random

import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Random.Class (MonadRandom)
import           Control.Monad.State.Strict (MonadState, StateT, evalStateT,
                                             runStateT)
import           Control.Monad.Trans.Class  (lift)
import           Data.Foldable              (foldr')
import           Data.Map.Strict            (Map)
import           System.ProgressBar

import           Board
import           Options
import           Play


type Weights = (Learner01.Weights', Learner02.Weights')


newtype App m a
  = App { unApp :: StateT GameHistory (StateT [Learner01.Choice'] (StateT [Learner02.Choice'] (StateT Weights m))) a }
  deriving
  ( Applicative
  , Functor
  , Monad
  , MonadIO
  , MonadRandom
  , MonadState GameHistory
  )

type SomePlayer = Player (App (StateT Stats IO ))


instance (Monad m) => Learner01.HasWeights' (App m) where
  readw = App $ lift $ lift $ lift $ State.gets fst

  writew ws = App $ lift $ lift $ lift $ do
    (_, ws2) <- State.get
    State.put (ws, ws2)

instance (Monad m) => Learner02.HasWeights' (App m) where
  readw = App $ lift $ lift $ lift $ State.gets snd

  writew ws = App $ lift $ lift $ lift $ do
    (ws1, _) <- State.get
    State.put (ws1, ws)

instance (Monad m) => Learner01.RecordsChoice' (App m) where
  recordChoice' = App . lift . Learner01.recordChoice'

instance (Monad m) => Learner02.RecordsChoice' (App m) where
  recordChoice' = App . lift . lift . Learner02.recordChoice'


runApp :: (Monad m) => App m a -> StateT Weights m (a, GameHistory, [Learner01.Choice'], [Learner02.Choice'])
runApp op = do
  (((a, b), c) ,d) <- runStateT ( runStateT (runStateT ( unApp op) mempty) mempty ) mempty
  pure (a, b, c, d)



type Stats = Map GameResult Int


updateStats :: (MonadState Stats m) => GameResult -> m ()
updateStats = State.modify . Map.alter (Just . (+) 1 . Maybe.fromMaybe 0)


run :: (MonadIO m, MonadState Stats m) => App m GameResult -> Weights -> m Weights
run game ws = do
  -- when (mod n 1000 == 0) $ Weights.writeWeightsToFile ws

  ((gameResult, gameHistory, choices01, choices02), updatedWeights) <- runStateT (runApp game) ws
  updateStats gameResult

  uw1 <- evalStateT (case gameResult of
    Winner X -> Learner01.increaseWeights choices01
    Winner O -> Learner01.decreaseWeights choices01
    Draw     -> Learner01.steadyWeights choices01
    _        -> pure mempty
            ) (fst updatedWeights)

  uw2 <- evalStateT (case gameResult of
    Winner X -> Learner02.recordXWin choices02
    Winner O -> Learner02.recordOWin choices02
    Draw     -> Learner02.recordDraw choices02
    _        -> pure mempty
            ) (snd updatedWeights)

  -- liftIO $ do
  --   printGame gameHistory

  --   print choices02
  --
  --   putStrLn $ case gameResult of
  --     Winner x   -> show x <> " wins!"
  --     NoPlayer x -> "no player found for " <> show x
  --     Draw       -> "Draw"
  --     NoMoves x  -> "no moves for " <> show x

  --   putStrLn ""


  pure (uw1, uw2)


runMany :: forall m. (MonadIO m, MonadState Stats m) => App m GameResult -> Int -> Weights -> m Weights
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
      -- liftIO $ print $ Aeson.encode $ fst ws'
      -- liftIO $ print $ Aeson.encode $ snd ws'
      f pb (x - 1) ws'


randomPlayer :: (MonadIO m) => Player (App m)
randomPlayer = Player Random.pickAny


minimaxPlayer :: (MonadRandom m) => Sign -> Player (App m)
minimaxPlayer = Player . Minimax.optimiseFor


perfectPlayer :: (MonadIO m) => Sign -> Player (App m)
perfectPlayer = Player . Perfect.bestPlay


learner01Player :: (MonadIO m) => Sign -> Player (App m)
learner01Player = Player . Learner01.selectMove


-- TODO: manage risk internally
learner02Player :: (MonadIO m) => Sign -> Player (App m)
learner02Player = Player . Learner02.adjustRisk 0.92




execute :: (SomePlayer, SomePlayer) -> Int -> IO ()
execute (xPlayer, oPlayer) n = do
  let players = Map.fromList [(X, xPlayer), (O, oPlayer)]
  let game = play players

  (_ws, stats) <- runStateT ( runMany game n mempty ) mempty

--   let (_, Learner02.Weights' w2) = ws
--    in liftIO $ print  w2

  print stats


main :: IO ()
main = do
  options <- readOptions
  print $ show (agentX options) <> " vs " <> show (agentO options)

  let xPlayer = selectAgent (agentX options) X
  let oPlayer = selectAgent (agentO options) O

  execute (xPlayer, oPlayer) (total options)

  where
    selectAgent :: AgentName -> Sign -> SomePlayer
    selectAgent Learner01 = learner01Player
    selectAgent Learner02 = learner02Player
    selectAgent Perfect   = perfectPlayer
    selectAgent Minimax   = minimaxPlayer
    selectAgent Random    = const randomPlayer
