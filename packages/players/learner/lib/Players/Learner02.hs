{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Players.Learner02
  ( Choice'
  , HasWeights' (..)
  , RecordsChoice' (..)
  , Weights' (..)
  , adjustRisk
  , recordDraw
  , recordOWin
  , recordXWin
  ) where

import qualified Control.Monad.State.Strict  as State
import qualified Control.Monad.Writer.Strict as Writer
import qualified Data.Aeson                  as Aeson
import qualified Data.List                   as List

import           Control.Monad.IO.Class      (MonadIO)
import           Control.Monad.State.Strict  (StateT, execStateT)
import           Control.Monad.Writer.Strict (WriterT)
import           GHC.Generics
import           Safe                        (headMay, lastMay)

import           Board
import           Players.Learner.Actions
import           Players.Learner.Choices
import           Players.Learner.Weights


newtype Weights'
  = Weights' Weights
  deriving (Aeson.ToJSON, Eq, Generic, Monoid, Semigroup, Show)


newtype Choice'
  = Choice' Choice
  deriving (Eq, Show)


class (Monad m) => RecordsChoice' m where
  recordChoice' :: Choice' -> m ()

instance (Monad m) => RecordsChoice' (StateT [Choice'] m) where
  recordChoice' choice = do
    choices <- State.get
    State.put $ choices <> pure choice

instance (Monad m) => RecordsChoice' (WriterT [Choice'] m) where
  recordChoice' = Writer.tell . pure



class (Monad m) => HasWeights' m where
  readw :: m Weights'
  writew :: Weights' -> m ()

instance (Monad m) => HasWeights' (StateT Weights' m) where
  readw = State.get
  writew = State.put


adjustRisk :: (MonadIO m, HasWeights' m, RecordsChoice' m) => Rational -> Sign -> Board -> m Move
adjustRisk risk sign board = do
  action <- pickAction risk

  case action of
    Explore -> do
      move <- pickAny board
      case move of
        (Just m) -> recordChoice' $ Choice' (Explore, board, m)
        _        -> pure ()
      pure move

    Exploit -> do
      (Weights' weights) <- readw
      let validMoves = getValidMoves board
      let weightedMoves = zip validMoves $ getWeight weights board <$> validMoves
      let moves = List.sortOn snd weightedMoves
      let selection = case sign of
                        X -> fst <$> lastMay moves
                        O -> fst <$> headMay moves
                        _ -> Nothing

      case selection of
        Nothing -> pure ()
        Just m  -> recordChoice' $ Choice' (Exploit, board, m)

      pure selection


recordXWin :: (HasWeights' m) => [Choice'] -> m Weights'
recordXWin xs = do
  (Weights' ws) <- readw
  Weights' <$> execStateT (recordXWin' xs) ws

  where
    recordXWin' :: (HasWeights m) => [Choice'] -> m Score
    recordXWin' [] = pure 1
    recordXWin' [Choice' y] = do
      modifyWeight (const 1) y
    recordXWin' ((Choice' y):ys) = do
      score <- recordXWin' ys
      modifyWeight (toward score) y


recordOWin :: (HasWeights' m) => [Choice'] -> m Weights'
recordOWin xs = do
  (Weights' ws) <- readw
  Weights' <$> execStateT (recordOWin' xs) ws

  where
    recordOWin' :: (HasWeights m) => [Choice'] -> m Score
    recordOWin' [] = pure 0
    recordOWin' [Choice' y] = do
      modifyWeight (const 0) y
    recordOWin' ((Choice' y):ys) = do
      score <- recordOWin' ys
      modifyWeight (toward score) y


recordDraw :: (HasWeights' m) => [Choice'] -> m Weights'
recordDraw xs = do
  (Weights' ws) <- readw
  Weights' <$> execStateT (recordDraw' xs) ws

  where
    recordDraw' :: (HasWeights m) => [Choice'] -> m Score
    recordDraw' [] = pure 0.5
    recordDraw' [Choice' y] = do
      modifyWeight (const 0.5) y
    recordDraw' ((Choice' y):ys) = do
      score <- recordDraw' ys
      modifyWeight (toward score) y
