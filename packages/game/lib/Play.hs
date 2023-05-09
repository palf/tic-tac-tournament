{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Play
  ( GameHistory
  , GameResult (..)
  , Player (..)
  , Players
  , play
  , printGame
  ) where

import qualified Control.Monad.State as State
import qualified Data.Foldable       as Foldable
import qualified Data.Sequence       as Seq

import           Control.Monad.State (MonadState)
import           Data.Map.Strict     (Map, (!?))
import           Data.Sequence       (Seq)

import           Board


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


play :: forall m. (MonadState GameHistory m) => Players m -> m GameResult
play players = play' X initialBoard

  where
    play' :: (Monad m, MonadState GameHistory m) => Sign -> Board -> m GameResult
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
                          in do
                            rs <- State.get
                            State.put (rs <> pure k)
                            play' ( nextSign sign ) k
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
