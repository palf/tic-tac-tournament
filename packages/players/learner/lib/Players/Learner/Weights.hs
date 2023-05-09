{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Players.Learner.Weights
  ( HasWeights (..)
  , Score
  , Weights
  , getWeight
  , modifyWeight
  , toward
  ) where

import qualified Control.Monad.State.Strict as State
import qualified Data.Map                   as Map
import qualified Data.Maybe                 as Maybe

import           Control.Monad.State.Strict (StateT)
import           Data.Map                   (Map)

import           Board
import           Players.Learner.Choices


type Score = Float


defaultWeightValue :: Float
defaultWeightValue = 0.5


type Weights = Map BoardKey (Map Position Score)


-- forall ws. (writeWeights ws >> readWeights) == ws
class (Monad m) => HasWeights m where
  readWeights :: m Weights
  writeWeights :: Weights -> m ()

instance (Monad m) => HasWeights (StateT Weights m) where
  readWeights = State.get
  writeWeights = State.put


getWeight :: Weights -> Board -> Position -> Float
getWeight weights board position = Maybe.fromMaybe defaultWeightValue $ do
  let (boardKey, rotInfo) = getBoardKey board
  xs <- Map.lookup boardKey weights

  let posTarget = identifyPosTarget rotInfo position
  let positionKey = posTarget
  Map.lookup positionKey xs


modifyWeight :: (HasWeights m) => (Score -> Score) -> Choice -> m Score
modifyWeight op choice = do
  weights <- readWeights

  let (_action, board, position) = choice
  let (boardKey, rotInfo) = getBoardKey board

  let positionKey = identifyPosTarget rotInfo position
  let (newWeights :: Weights)
        = Map.alter
          ( Just . Map.alter
            ( Just . op . Maybe.fromMaybe defaultWeightValue
            ) positionKey . Maybe.fromMaybe mempty
          ) boardKey weights

  writeWeights newWeights

  let (value :: Score) = getWeight newWeights board position
  pure value


toward :: (Num a, Fractional a) => a -> a -> a
toward n x = x + difference * step
  where
    difference = n - x
    step = 0.5
