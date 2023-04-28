{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified Data.Map.Strict           as Map
import qualified Players.Learner           as Learner
import qualified Players.Learner.Weights   as Learner

import           Board
import           Control.Monad.State       (StateT, runStateT)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Writer      (WriterT, runWriterT)
import           Play


type RememberChoice = StateT [Learner.Choice]
type RecordGame = WriterT GameHistory

type App = (RecordGame (RememberChoice (RememberChoice IO)))


runApp :: App GameResult -> IO (((GameResult, GameHistory), [Learner.Choice]), [Learner.Choice])
runApp op = runStateT (runStateT ( runWriterT op) mempty) mempty


instance Learner.HasWeights (RememberChoice IO) where
  readWeights = Learner.readWeightsFromFile
  writeWeights = Learner.writeWeightsToFile


main :: IO ()
main = do
  let (xPlayer :: Player App) = Player $ lift . Learner.optimiseFor X
  let (oPlayer :: Player App) = Player $ lift . lift . Learner.optimiseFor O

  let players = Map.fromList [(X, xPlayer), (O, oPlayer)]
  let op :: App GameResult = play1 players

  (((end, game), choicesX), choicesO) <- runApp op

  printGame game

  putStrLn $ case end of
    Winner x   -> show x <> " wins!"
    NoPlayer x -> "no player found for " <> show x
    Draw       -> "Draw"
    NoMoves x  -> "no moves for " <> show x

  case end of
    Winner X -> Learner.increaseWeights choicesX >> Learner.decreaseWeights choicesO
    Winner O -> Learner.increaseWeights choicesO >> Learner.decreaseWeights choicesX
    Draw -> Learner.steadyWeights choicesO >> Learner.steadyWeights choicesX
    _ -> pure ()
