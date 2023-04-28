{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified Data.Map.Strict           as Map
import qualified Players.Learner           as Learner
import qualified Players.Perfect           as Perfect
import qualified Players.Random            as Random

import           Board
import           Control.Monad.State       (runStateT)
import           Control.Monad.Trans.Class
import           Control.Monad.Writer      (runWriterT)
import           Play


main :: IO ()
main = do
  let xPlayer = Player (lift . Learner.optimiseFor X)
  let oPlayer = Player $ Perfect.bestPlay O

  let players = Map.fromList [(X, xPlayer), (O, oPlayer)]

  ((end, game), choices) <- runStateT (runWriterT ( play1 players )) mempty

  printGame game

  putStrLn $ case end of
    Winner x   -> show x <> " wins!"
    NoPlayer x -> "no player found for " <> show x
    Draw       -> "Draw"
    NoMoves x  -> "no moves for " <> show x

  case end of
    Winner X -> Learner.increaseWeights choices
    Draw     -> Learner.increaseWeights choices
    _        -> Learner.decreaseWeights choices
