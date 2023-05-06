{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Players.Learner01
  ( Choice'
  , HasWeights' (..)
  , RecordsChoice' (..)
  , Weights' (..)
  , decreaseWeights
  , increaseWeights
  , recordChoice
  , selectMove
  , steadyWeights
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


selectMove :: (MonadIO m, HasWeights' m, RecordsChoice' m) => Sign -> Board -> m Move
selectMove sign board = do
  action <- pickAction 0.9

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

      pure selection



increaseWeights :: (HasWeights' m) => [Choice'] -> m Weights'
increaseWeights xs = do
  (Weights' ws) <- readw
  Weights' <$> execStateT (mapM_ f xs) ws
    where
      f (Choice' x) = modifyWeight (toward 1) x


decreaseWeights :: (HasWeights' m) => [Choice'] -> m Weights'
decreaseWeights xs = do
  (Weights' ws) <- readw
  Weights' <$> execStateT (mapM_ f xs) ws
    where
      f (Choice' x) = modifyWeight (toward 0) x


steadyWeights :: (HasWeights' m) => [Choice'] -> m Weights'
steadyWeights xs = do
  (Weights' ws) <- readw
  Weights' <$> execStateT (mapM_ f xs) ws
    where
      f (Choice' x) = modifyWeight (toward 0.5) x
