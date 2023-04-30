{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Play
  ( GameHistory
  , GameResult (..)
  , Player (..)
  , play
  , play1
  , printGame
  ) where

import qualified Control.Monad.Writer as Writer
import qualified Data.Foldable        as Foldable
import qualified Data.Sequence        as Seq

import           Board
import           Control.Monad.Writer (MonadWriter, runWriterT)
import           Data.Map.Strict      (Map, (!?))
import           Data.Sequence        (Seq)


type GameHistory = Seq Board


newtype Player m
  = Player (Board -> m (Maybe Position))


data GameResult
  = Winner Sign
  | Draw
  | NoMoves Sign
  | NoPlayer Sign
  deriving (Eq, Ord, Show)


type Players m = Map Sign (Player m)

-- play :: (Monad m, MonadTrans t, MonadWriter GameHistory (t m)) => Players (t m) -> m (GameResult, GameHistory)
play players = runWriterT $ play1 players


play1 :: forall m. (MonadWriter GameHistory m) => Players m -> m GameResult
-- play1 players = play' X $ createBoard [X, Empty, Empty, Empty, O, O, Empty, Empty, X]
play1 players = play' X initialBoard

  where
    play' :: (Monad m, MonadWriter GameHistory m) => Sign -> Board -> m GameResult
    play' sign board
      | Just winner <- findWinner board
        = pure (Winner winner)

      | boardIsFull board
        = pure Draw

      | otherwise
        = maybe
            ( pure $ NoPlayer sign )
            ( \(Player mv) -> mv board >>= maybe
                ( pure $ NoMoves sign )
                ( \x -> let k = makeMove board sign x
                          in Writer.tell (pure k) >> play' ( nextSign sign ) k
                )
            )
            ( players !? sign )


printGame :: GameHistory -> IO ()
printGame
  = mapM_ putStrLn
  . Seq.fromList
  . fmap (foldl1 tab)
  . collectLines
  . Foldable.toList
  . fmap (lines . renderBoard)

  where
    collectLines :: [[String]] -> [[String]]
    collectLines [] = []
    collectLines xs =
        [ fmap (!! 0) xs
        , fmap (!! 1) xs
        , fmap (!! 2) xs
        ]

    tab :: String -> String -> String
    tab x y = x <> "  :  " <> y
