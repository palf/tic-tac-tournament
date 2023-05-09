module Players.Learner.File
  ( readWeightsFromFile
  , writeWeightsToFile
  ) where

import qualified Data.Aeson               as Aeson
import qualified Data.Aeson.Encode.Pretty as Pretty
import qualified Data.ByteString.Lazy     as Lazy

import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           System.Directory         (doesFileExist)

import           Players.Learner.Weights  (Weights)


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
