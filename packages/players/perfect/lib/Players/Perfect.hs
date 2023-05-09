{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Players.Perfect where

import qualified Control.Monad.Random   as Random
import qualified Data.List              as List
import qualified Data.Maybe             as Maybe

import           Control.Monad
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Text              (Text)

import           Board
import           Play


randomElementIO :: [a] -> IO (Maybe a)
randomElementIO [] = pure Nothing
randomElementIO xs = do
  idx <- Random.randomRIO (0, Prelude.length xs - 1)
  pure $ Just (xs !! idx)


pickAny :: (MonadIO m) => Board -> m Move
pickAny = liftIO . randomElementIO . getValidMoves


bestPlay :: (MonadIO m) => Sign -> Board -> m Move
bestPlay playerSign board = do
  let (allowed :: [Position])= getValidMoves board

  if length allowed == 1
    then pure (Just $ head allowed)
    else do
      let hasWinner = findWinner . makeMove board playerSign
      let winner = List.find (Maybe.isJust . hasWinner) allowed

      let hasLoser = findWinner . makeMove board (nextSign playerSign)
      let loser = List.find (Maybe.isJust . hasLoser) allowed

      if Maybe.isJust winner
        then pure winner
        else do
          if Maybe.isJust loser
            then pure loser
            else do
              let (BoardKey boardKey, transform) = getBoardKey board
              let goodMoves = selectPositions boardKey

              let (positions :: [Position]) = (identifyPosSource transform) <$> goodMoves

              when (null positions) $ liftIO $ print $ "no moves for: " <> show boardKey

              let disallowed = List.filter (`notElem` allowed) positions
              unless (null disallowed) $ liftIO $ do
                print ("allowed"    :: String, allowed)
                print ("disallowed" :: String, disallowed)
                print ("boardKey"   :: String, boardKey)
                print ("transform"  :: String, transform)
                print ("good moves" :: String, goodMoves)
                print ("positions"  :: String, positions)
                printGame $ pure board
                error "move disallowed"

              liftIO $ randomElementIO positions


selectPositions :: Text -> [Position]
selectPositions "   |   |   " = [A1, A2, A3, B1, B2, B3, C1, C2, C3]
selectPositions "   |   |  X" = [B2]
selectPositions "   |   | OX" = [A1, A2, A3, B1, B2, B3, C1]
selectPositions "   |   | X " = [B2, C1, C3]
selectPositions "   |   | XO" = [A1, A3, B2]
selectPositions "   |   |O X" = [A1, A2, A3, B1, B2, B3]
selectPositions "   |   |OXX" = [A1, A2, B1, B2]
selectPositions "   |  O| X " = [A1, A3, B1, B2, C3]
selectPositions "   |  O|OXX" = [B1, B2]
selectPositions "   |  O|X  " = [A1, A2, A3, B1, B2, C3]
selectPositions "   |  O|XOX" = [A1, A2, B1, B2]
selectPositions "   |  O|XX " = [C3]
selectPositions "   |  O|XXO" = [A3]
selectPositions "   |  X| XO" = [A2, B1]
selectPositions "   |  X|O  " = [A1, B2, C3]
selectPositions "   |  X|O X" = [A3]
selectPositions "   |  X|OX " = [A1]
selectPositions "   |  X|X O" = [B1, B2]
selectPositions "   |  X|XO " = [A1, A2, A3, B1, B2, C3]
selectPositions "   |  X|XOO" = [A1, A2, B2]
selectPositions "   | O |  X" = [A1]
selectPositions "   | O | X " = [A1, A3, B1, B3, C1, C3]
selectPositions "   | O | XX" = [C1]
selectPositions "   | O |OXX" = [A3]
selectPositions "   | OX| X " = [A3, C1, C3]
selectPositions "   | OX| XO" = [A1]
selectPositions "   | OX|OX " = [A3]
selectPositions "   | OX|X  " = [A2, A3, C2, C3]
selectPositions "   | OX|X O" = [A1]
selectPositions "   | OX|XO " = [A2]
selectPositions "   | X |   " = [A1, A3, C1, C3]
selectPositions "   | X |  O" = [B3, C2]
selectPositions "   | X | O " = [A2]
selectPositions "   | X | OX" = [A1]
selectPositions "   | X | XO" = [A2]
selectPositions "   | X |O X" = [A1]
selectPositions "   | XO| X " = [A2]
selectPositions "   | XO|OX " = [A2]
selectPositions "   | XO|X  " = [A3]
selectPositions "   | XX|O  " = [B1]
selectPositions "   | XX|OO " = [C3]
selectPositions "   |O X|   " = [A1, A2, A3, B2, C1, C2, C3]
selectPositions "   |O X|  X" = [A3]
selectPositions "   |O X| X " = [C3]
selectPositions "   |O X| XO" = [A1, A2, B2, C1]
selectPositions "   |O X|X  " = [C3]
selectPositions "   |O X|X O" = [A1, A2, A3, B2, C2]
selectPositions "   |O X|XO " = [A1, A2, A3, B2, C3]
selectPositions "   |O X|XOX" = [A3]
selectPositions "   |O X|XXO" = [B2]
selectPositions "   |OOX| X " = [C3]
selectPositions "   |OOX| XX" = [A3, C1]
selectPositions "   |OOX|OXX" = [A3]
selectPositions "   |OOX|X  " = [C3]
selectPositions "   |OOX|X X" = [A3]
selectPositions "   |OOX|XOX" = [A3]
selectPositions "   |OOX|XXO" = [A1]
selectPositions "   |OXX|   " = [A1, A3, C1, C3]
selectPositions "   |OXX|  O" = [A1, A2, C1, C2]
selectPositions "   |OXX| O " = [C3]
selectPositions "   |OXX| OX" = [A1, A3]
selectPositions "   |OXX| XO" = [A2]
selectPositions "   |OXX|O  " = [A1]
selectPositions "   |OXX|X O" = [A3]
selectPositions "   |OXX|XO " = [A3]
selectPositions "   |X X|  O" = [B2]
selectPositions "   |X X| O " = [B2]
selectPositions "   |XOX|   " = [A1, A2, A3, C1, C2, C3]
selectPositions "   |XOX|  O" = [A1]
selectPositions "   |XOX| O " = [A2]
selectPositions "  O|   |OXX" = [B2]
selectPositions "  O|   |X  " = [A1, A2, B2, B3, C3]
selectPositions "  O|   |XOX" = [A1]
selectPositions "  O|   |XX " = [C3]
selectPositions "  O|   |XXO" = [B3]
selectPositions "  O|  X|X  " = [A1, B1, B2]
selectPositions "  O|  X|X O" = [B1]
selectPositions "  O|  X|XO " = [B1]
selectPositions "  O|  X|XOX" = [A1, A2, B1, B2]
selectPositions "  O|  X|XXO" = [A1, A2, B1, B2]
selectPositions "  O| OX|X  " = [A1, A2, C2, C3]
selectPositions "  O| OX|X X" = [C2]
selectPositions "  O| OX|XOX" = [A2]
selectPositions "  O| OX|XX " = [C3]
selectPositions "  O| OX|XXO" = [A1]
selectPositions "  O| X |OXX" = [A1, A2]
selectPositions "  O| X |X  " = [A1, C3]
selectPositions "  O| X |XO " = [A1, A2, B1, B3, C3]
selectPositions "  O|O X|  X" = [A1, C1]
selectPositions "  O|O X| X " = [A1, A2, B2, C1]
selectPositions "  O|O X| XX" = [C1]
selectPositions "  O|O X|OXX" = [A1]
selectPositions "  O|O X|X  " = [A1, A2, B2, C2, C3]
selectPositions "  O|O X|X X" = [C2]
selectPositions "  O|O X|XOX" = [A1, A2, B2]
selectPositions "  O|O X|XX " = [C3]
selectPositions "  O|O X|XXO" = [A1, A2, B2]
selectPositions "  O|OOX|X X" = [C2]
selectPositions "  O|OXX|  X" = [A1]
selectPositions "  O|OXX| OX" = [A1, A2, C1]
selectPositions "  O|OXX| X " = [A2]
selectPositions "  O|OXX|O X" = [A1]
selectPositions "  O|OXX|X  " = [A1]
selectPositions "  O|OXX|X O" = [A1, A2, C2]
selectPositions "  O|OXX|XO " = [A1, A2, C3]
selectPositions "  O|OXX|XOX" = [A1]
selectPositions "  O|OXX|XXO" = [A2]
selectPositions "  O|X  |  X" = [A1, B2]
selectPositions "  O|X  | OX" = [B2]
selectPositions "  O|X  | X " = [A1, A2, B2, B3, C1, C3]
selectPositions "  O|X  | XO" = [B3]
selectPositions "  O|X  |O X" = [B2]
selectPositions "  O|X O|  X" = [A1, C1]
selectPositions "  O|X O|X X" = [A1, C2]
selectPositions "  O|X X| XO" = [B2]
selectPositions "  O|X X|XO " = [A1]
selectPositions "  O|XO |  X" = [C1]
selectPositions "  O|XO | X " = [C1]
selectPositions "  O|XO |X X" = [A1, C2]
selectPositions "  O|XO |XOX" = [A1]
selectPositions "  O|XO |XX " = [A1, C3]
selectPositions "  O|XO |XXO" = [A1]
selectPositions "  O|XOX| X " = [C1]
selectPositions "  O|XOX| XO" = [A1, C1]
selectPositions "  O|XOX|X  " = [A1]
selectPositions "  O|XOX|XO " = [A1]
selectPositions "  O|XOX|XOX" = [A2]
selectPositions "  O|XOX|XXO" = [A1]
selectPositions "  O|XX | OX" = [A1, B3]
selectPositions "  O|XX |O X" = [A1, B3]
selectPositions "  O|XXO|  X" = [A1]
selectPositions "  O|XXO| OX" = [A1]
selectPositions "  O|XXO|O X" = [A1]
selectPositions "  X|  O|XXO" = [B2]
selectPositions "  X| O |X  " = [A2, B1, B3, C2]
selectPositions "  X| O |XO " = [A2]
selectPositions "  X| O |XOX" = [A2]
selectPositions "  X| OO|XX " = [C3]
selectPositions "  X|O  | X " = [B2, C1]
selectPositions "  X|O  | XO" = [A2]
selectPositions "  X|O  |OX " = [A1]
selectPositions "  X|O  |OXX" = [B3]
selectPositions "  X|O  |XXO" = [B2]
selectPositions "  X|O O|OXX" = [B2]
selectPositions "  X|O X| XO" = [A1, A2, B2, C1]
selectPositions "  X|O X|OX " = [C3]
selectPositions "  X|O X|OXO" = [A1]
selectPositions "  X|O X|X O" = [B2]
selectPositions "  X|O X|XO " = [C3]
selectPositions "  X|O X|XOO" = [B2]
selectPositions "  X|OO | X " = [B3]
selectPositions "  X|OO |XX " = [B3]
selectPositions "  X|OO |XXO" = [A1, B3]
selectPositions "  X|OOX| X " = [C3]
selectPositions "  X|OOX| XO" = [A1]
selectPositions "  X|OOX|X  " = [C3]
selectPositions "  X|OOX|X O" = [A1]
selectPositions "  X|OOX|XXO" = [B3]
selectPositions "  X|OXO|OXX" = [A2]
selectPositions "  X|OXX|O O" = [C2]
selectPositions "  X|OXX|OXO" = [A2]
selectPositions "  X|OXX|X O" = [A1, A2, C2]
selectPositions "  X|X  |O O" = [C2]
selectPositions "  X|X  |OOX" = [B3]
selectPositions "  X|X  |OXO" = [A2, B2]
selectPositions "  X|X O| O " = [A1, C1]
selectPositions "  X|X O|O X" = [A1, A2, B2]
selectPositions "  X|X O|OOX" = [A1]
selectPositions "  X|X O|OX " = [A1, A2, B2]
selectPositions "  X|X O|OXO" = [A2]
selectPositions "  X|X O|X O" = [A1]
selectPositions "  X|X X|OO " = [B2, C1]
selectPositions "  X|XO |O X" = [B3]
selectPositions "  X|XO |OX " = [A1, A2, B3, C3]
selectPositions "  X|XO |OXO" = [A1]
selectPositions "  X|XOO|  X" = [A1, A2, C1, C2]
selectPositions "  X|XOO| X " = [A1, C1]
selectPositions "  X|XOO| XO" = [A1]
selectPositions "  X|XOO|O X" = [A1, A2, C2]
selectPositions "  X|XOO|OX " = [A1, A2, C3]
selectPositions "  X|XOO|OXX" = [A1, A2]
selectPositions "  X|XOO|X O" = [A1]
selectPositions "  X|XOO|XXO" = [A1]
selectPositions "  X|XOX|O O" = [A1, C2]
selectPositions "  X|XOX|OXO" = [A1]
selectPositions "  X|XXO|O O" = [C2]
selectPositions "  X|XXO|OXO" = [A2]
selectPositions " O |O X| X " = [C3]
selectPositions " O |O X|X X" = [A3]
selectPositions " O |O X|XXO" = [A1, A3, B2]
selectPositions " O |OXX| XO" = [A1, A3, C1]
selectPositions " O |OXX|OX " = [A1]
selectPositions " O |OXX|OXX" = [A1, A3]
selectPositions " O |OXX|XXO" = [A3]
selectPositions " O |X X| XO" = [B2]
selectPositions " O |X X|O X" = [B2]
selectPositions " O |XOX| X " = [A1, A3, C1, C3]
selectPositions " O |XOX| XO" = [A1]
selectPositions " O |XOX|O X" = [A3, C2]
selectPositions " O |XOX|OXX" = [A1, A3]
selectPositions " OO|X  |OXX" = [A1]
selectPositions " OO|X X|XXO" = [A1]
selectPositions " OO|XOX| X " = [A1]
selectPositions " OX|OXX| XO" = [A1, C1]
selectPositions " OX|X  | XO" = [B2, C1]
selectPositions " OX|X  |OXO" = [A1, B2, B3]
selectPositions " OX|X O| X " = [A1, B2, C1, C3]
selectPositions " OX|X O| XO" = [C1]
selectPositions " OX|X O|O X" = [A1, B2, C2]
selectPositions " OX|X O|OX " = [A1, B2, C3]
selectPositions " OX|X O|OXX" = [A1, B2]
selectPositions " OX|X O|XXO" = [B2]
selectPositions " OX|X X|O O" = [B2]
selectPositions " OX|X X|OXO" = [B2]
selectPositions " OX|XO | XO" = [A1]
selectPositions " OX|XO |OXX" = [B3]
selectPositions " OX|XOO| X " = [C1]
selectPositions " OX|XOO|XX " = [A1, C3]
selectPositions " OX|XOX| XO" = [A1, C1]
selectPositions " OX|XOX|OX " = [C3]
selectPositions " OX|XX |O O" = [B2]
selectPositions " OX|XX |OXO" = [B3]
selectPositions " OX|XXO| XO" = [C1]
selectPositions " OX|XXO|O X" = [A1]
selectPositions " OX|XXO|OX " = [A1, C3]
selectPositions " X |XOX| O " = [A1, A3, C1, C3]
selectPositions " XO|X  |OOX" = [B2]
selectPositions " XO|XO |XOX" = [A1]
selectPositions " XO|XOO|X X" = [A1, C2]
selectPositions " XO|XX |OOX" = [A1, B3]
selectPositions "O O|  X|XXO" = [A2, B2]
selectPositions "O O| X |OXX" = [A2, B1]
selectPositions "O O|OXX|X O" = [A2]
selectPositions "O O|XOX|X X" = [A2]
selectPositions "O X|O X|XXO" = [B2]
selectPositions "O X|X  |O X" = [B3]
selectPositions "O X|X O|O X" = [A2, B2, C2]
selectPositions "O X|X O|OXX" = [A2, B2]
selectPositions "O X|X O|XOX" = [B2]
selectPositions "O X|X O|XXO" = [B2]
selectPositions "O X|XO |OXX" = [B3]
selectPositions "O X|XO |XOX" = [A2]
selectPositions "O X|XOO|X X" = [C2]
selectPositions "O X|XXO|O X" = [A2, C2]
selectPositions "OOX|X  |OXX" = [B3]
selectPositions "OOX|X O|X X" = [B2, C2]
selectPositions "OOX|XO |X X" = [C2]

selectPositions _ = []
