module Players.Random (pickAny) where

import qualified System.Random          as Random

import           Board
import           Control.Monad.IO.Class (MonadIO, liftIO)


randomElementIO :: [a] -> IO (Maybe a)
randomElementIO [] = pure Nothing
randomElementIO xs = do
  idx <- Random.randomRIO (0, length xs - 1)
  pure $ Just (xs !! idx)


pickAny :: (MonadIO m) => Board -> m (Maybe Position)
pickAny = liftIO . randomElementIO . getValidMoves
