{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified Data.Map.Strict            as Map
import qualified Players.Learner            as Learner
import qualified Players.Random             as Random

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Writer

import           Board
import           Control.Monad              ()
import           Control.Monad.Writer       (runWriterT)
import           Play


-- type ThisPlayer = Player (WriterT GameHistory (StateT [Learner.Choice] IO))


main :: IO ()
main = do
  let xPlayer = Player (lift . Learner.optimiseFor X)
  -- let (oPlayer :: Player IO) = Player Random.pickAny
  let oPlayer = Player Random.pickAny

  let players = Map.fromList [(X, xPlayer), (O, oPlayer)]

  ((end, game), choices) <- runStateT ( runWriterT ( play1 players )) []

  printGame game
  print $ fmap (boardToString . fst) choices
  print $ fmap snd choices

  putStrLn $ case end of
    Winner x   -> show x <> " wins!"
    NoPlayer x -> "no player found for " <> show x
    Draw       -> "Draw"
    NoMoves x  -> "no moves for " <> show x

  case end of
    Winner X -> Learner.increaseWeights choices
    Winner O -> Learner.decreaseWeights choices
    _        -> pure ()
