{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import qualified Data.Map.Strict as Map
import qualified Players.Random  as Random

import           Board
import           Play



main :: IO ()
main = do
  let xPlayer = Player Random.pickAny
  let oPlayer = Player Random.pickAny

  let players = Map.fromList [(X, xPlayer), (O, oPlayer)]

  (result, game) <- play players

  printGame game

  putStrLn $ case result of
    Winner x   -> show x <> " wins!"
    NoPlayer x -> "no player found for " <> show x
    Draw       -> "Draw"
    NoMoves x  -> "no moves for " <> show x
