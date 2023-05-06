{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module Players.Learner.Choices where

import qualified Control.Monad.State.Strict  as State
import qualified Control.Monad.Writer.Strict as Writer

import           Control.Monad.State.Strict  (StateT)
import           Control.Monad.Writer.Strict (WriterT)

import           Board
import           Players.Learner.Actions


type Choice = (Action, Board, Position)


class (Monad m) => RecordsChoice m where
  recordChoice :: Choice -> m ()


instance (Monad m) => RecordsChoice (StateT [Choice] m) where
  recordChoice choice = do
    choices <- State.get
    State.put $ choices <> pure choice


instance (Monad m) => RecordsChoice (WriterT [Choice] m) where
  recordChoice = Writer.tell . pure
