{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import qualified Data.Map.Strict as Map
import qualified Players.Minimax as Minimax
import qualified Players.Random  as Random

import           Board
import           Play


main :: IO ()
main = do
  let xPlayer = Player (Minimax.optimiseFor X)
  let oPlayer = Player Random.pickAny

  let players = Map.fromList [(X, xPlayer), (O, oPlayer)]

  (end, game) <- play players

  printGame game

  putStrLn $ case end of
    Winner x   -> show x <> " wins!"
    NoPlayer x -> "no player found for " <> show x
    Draw       -> "Draw"
    NoMoves x  -> "no moves for " <> show x
