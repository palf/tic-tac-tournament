{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Players.Learner.Weights where

import qualified Control.Monad.State      as State
import qualified Control.Monad.Writer     as Writer
import qualified Data.Aeson               as Aeson
import qualified Data.Aeson.Encode.Pretty as Pretty
import qualified Data.ByteString.Lazy     as Lazy
import qualified Data.Map                 as Map
import qualified Data.Maybe               as Maybe

import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Control.Monad.State      (MonadState, StateT)
import           Control.Monad.Writer     (MonadWriter, WriterT)
import           Data.Map                 (Map)
import           System.Directory         (doesFileExist)

import           Board


type BoardString = String
type PositionString = String
type Weights = Map BoardString (Map PositionString Float)


defaultWeightValue :: Float
defaultWeightValue = 0.5


weightsFile :: FilePath
weightsFile = "weights.json"


readWeightsFromFile :: (MonadIO m) => m Weights
readWeightsFromFile = liftIO $ do
  exists <- doesFileExist weightsFile
  if exists
    then do
      x <- Aeson.eitherDecode <$> Lazy.readFile weightsFile
      either mempty pure x

    else do
      pure mempty


writeWeightsToFile :: (MonadIO m) => Weights -> m ()
writeWeightsToFile = liftIO . Lazy.writeFile weightsFile . Pretty.encodePretty


getWeight :: Weights -> Board -> Position -> Float
getWeight weights board position = Maybe.fromMaybe defaultWeightValue $ do
  let (boardKey, rotInfo) = bestBoardKey board
  xs <- Map.lookup boardKey weights

  let positionKey = positionToString (unrotatePosition rotInfo position)
  Map.lookup positionKey xs


type Choice = (Board, Move)


modifyWeight :: (MonadIO m, HasWeights m) => (Float -> Float) -> Choice -> m ()
modifyWeight op choice = do
  weights <- readWeights

  let (board, Just position) = choice
  let (boardKey, rotInfo) = bestBoardKey board

  let (posWeights :: Map PositionString Float) =
        Maybe.fromMaybe mempty $ Map.lookup boardKey weights

  let positionKey = positionToString (unrotatePosition rotInfo position)

  let (value :: Float) =
        Maybe.fromMaybe defaultWeightValue $ Map.lookup positionKey posWeights

  let newValue = op value

  let (newPosWeights :: Map PositionString Float) =
        Map.alter (\_ -> Just newValue) positionKey posWeights

  let (newWeights :: Weights) =
        Map.alter (\_ -> Just newPosWeights) boardKey weights

  liftIO $ print
    ( "modify"
    , ("choice", choice)
    , ( "board", board)
    , ( "boardKey", boardKey)
    , ( "rotInfo", rotInfo)
    , ( "posWeights", posWeights)
    , ( "positionKey", positionKey)
    , ( "value", value)
    )

  writeWeights newWeights


increaseWeights :: (MonadIO m, HasWeights m) => [Choice] -> m ()
increaseWeights = mapM_ (modifyWeight increase)
  where
    increase x = min 1 (x + 0.1)


decreaseWeights :: (MonadIO m, HasWeights m) => [Choice] -> m ()
decreaseWeights = mapM_ (modifyWeight decrease)
  where
    decrease x = max 0 (x - 0.1)


steadyWeights :: (MonadIO m, HasWeights m) => [Choice] -> m ()
steadyWeights = mapM_ (modifyWeight steady)
  where
    steady x = x + ((0.5 - x) / 2)


class HasWeights m where
  readWeights :: m Weights
  writeWeights :: Weights -> m ()


instance HasWeights IO where
  readWeights = readWeightsFromFile
  writeWeights = writeWeightsToFile
