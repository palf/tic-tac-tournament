{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified Data.Map.Strict           as Map
import qualified Players.Perfect           as Perfect
import qualified Players.Random            as Random

import           Board
import           Control.Monad.Trans.Class
import           Control.Monad.Writer      (runWriterT)
import           Play


main :: IO ()
main = do
  let xPlayer = Player Random.pickAny
  let oPlayer = Player $ Perfect.bestPlay X

  let players = Map.fromList [(X, xPlayer), (O, oPlayer)]

  (end, game) <- runWriterT ( play1 players )

  printGame game

  case end of
    Winner X  -> error "random should not win"
    NoMoves _ -> error "no moves"
    _         -> pure ()

  putStrLn $ case end of
    Winner x   -> show x <> " wins!"
    NoPlayer x -> "no player found for " <> show x
    Draw       -> "Draw"
    NoMoves x  -> "no moves for " <> show x
