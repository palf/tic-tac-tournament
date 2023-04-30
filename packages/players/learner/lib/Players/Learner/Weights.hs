{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DeriveGeneric       #-}
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
import           GHC.Generics
import           System.Directory         (doesFileExist)

import           Board


type BoardString = String
type PositionString = String

data Report
  = Report
    { played :: Int
    , won    :: Int
    , drawn  :: Int
    , lost   :: Int
    }
  deriving (Eq, Generic, Show)


instance Aeson.ToJSON Report
instance Aeson.FromJSON Report


emptyReport :: Report
emptyReport = Report 0 0 0 0


type Weights = Map BoardString (Map PositionString Report)


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

  let positionKey = positionToString (applyTransformPos rotInfo position)
  report <- Map.lookup positionKey xs

  if played report < 10
     then Nothing
     else if won report == 0
       then pure 0
       else if lost report == 0
         then pure 1
         else Just $ fromIntegral (won report) / fromIntegral (played report)


type Choice = (Board, Position)

modifyWeight :: (MonadIO m, HasWeights m) => (Report -> Report) -> Choice -> m ()
modifyWeight op choice = do
  weights <- readWeights

  let (board, position) = choice
  let (boardKey, rotInfo) = bestBoardKey board

  let (posWeights :: Map PositionString Report) =
        Maybe.fromMaybe mempty $ Map.lookup boardKey weights

  let positionKey = positionToString (applyTransformPos rotInfo position)

  let (value :: Report) =
        Maybe.fromMaybe emptyReport $ Map.lookup positionKey posWeights

  let newValue = op value

  let (newPosWeights :: Map PositionString Report) =
        Map.alter (\_ -> Just newValue) positionKey posWeights

  let (newWeights :: Weights) =
        Map.alter (\_ -> Just newPosWeights) boardKey weights

  writeWeights newWeights


increase x = x { played = played x + 1, won = won x + 1 }
decrease x = x { played = played x + 1, lost = lost x + 1 }
steady x = x { played = played x + 1, drawn = drawn x + 1 }


increaseWeights :: (MonadIO m, HasWeights m) => [Choice] -> m ()
increaseWeights = mapM_ (modifyWeight increase)



decreaseWeights :: (MonadIO m, HasWeights m) => [Choice] -> m ()
decreaseWeights = mapM_ (modifyWeight decrease)


steadyWeights :: (MonadIO m, HasWeights m) => [Choice] -> m ()
steadyWeights = mapM_ (modifyWeight steady)


class HasWeights m where
  readWeights :: m Weights
  writeWeights :: Weights -> m ()
