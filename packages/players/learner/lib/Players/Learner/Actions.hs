module Players.Learner.Actions where

import qualified Control.Monad.Random   as Random

import           Control.Monad.IO.Class (MonadIO, liftIO)

import           TicTacTournament


data Action = Exploit | Explore deriving (Eq, Show)


randomElementIO :: [a] -> IO (Maybe a)
randomElementIO [] = pure Nothing
randomElementIO xs = do
  idx <- Random.randomRIO (0, Prelude.length xs - 1)
  pure $ Just (xs !! idx)


pickAny :: (MonadIO m) => Board -> m (Maybe Position)
pickAny = liftIO . randomElementIO . getValidMoves


pickAction :: (MonadIO m) => Rational -> m Action
pickAction n
  | n < 0 = error $ "invalid n < 0: " <> show n
  | n > 1 = error "invalid n > 1"
  | otherwise = liftIO $ Random.weighted [(Exploit, n), (Explore, 1 - n)]
