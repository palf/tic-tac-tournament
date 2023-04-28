{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Players.Perfect where

import qualified Control.Monad.Except   as Except
import qualified Control.Monad.Random   as Random
import qualified Data.List              as List
import qualified Data.List.Split        as Split
import qualified Data.Maybe             as Maybe

import           Control.Monad          (unless)
import           Control.Monad.Except   (Except, runExcept)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Either

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
              let (s , r ) = bestRotation board
              let (thing :: [Position]) = fromRight allowed $
                    runExcept (fmap (unrotatePosition r) <$> selectPositions s)

              let disallowed = List.filter (`notElem` allowed) thing
              unless (null disallowed) $ liftIO $ do
                print ("a", allowed)
                print ("d", disallowed)
                print ("s", s)
                print ("r", r)
                print ("t", thing)
                printGame $ pure board
                error "move disallowed"

              liftIO $ randomElementIO thing


selectPositions :: String -> Except String [Position]
selectPositions "         " = pure [A1, A2, A3, B1, B2, B3, C1, C2, C3]
selectPositions "        X" = pure [B2]
selectPositions "       X " = pure [B2, C1, C3]
selectPositions "       XO" = pure [A1, A3, B2]
selectPositions "      O X" = pure [A1, A2, A3, B1, B2, B3]
selectPositions "      OXX" = pure [A1, A2, A3, B1, B2, B3]
selectPositions "     O X " = pure [A1, A3, B1, B2, C3]
selectPositions "     O XX" = pure [C1]
selectPositions "     OOXX" = pure [B2]
selectPositions "     OX  " = pure [A1, A2, A3, B1, B2, C3]
selectPositions "     OXOX" = pure [A1, A2, B1, B2]
selectPositions "     OXX " = pure [C3]
selectPositions "     OXXO" = pure [A3]
selectPositions "     X XO" = pure [A2, B1]
selectPositions "     XO  " = pure [A1, B2, C3]
selectPositions "     XO X" = pure [A3]
selectPositions "     XOX " = pure [A1]
selectPositions "     XX O" = pure [B1, B2]
selectPositions "     XXO " = pure [A1, A2, A3, B1, B2, C3]
selectPositions "     XXOO" = pure [A1, A2, B2]
selectPositions "    O   X" = pure [A1]
selectPositions "    O  X " = pure [A1, A3, B1, B3, C1, C3]
selectPositions "    O  XX" = pure [C1]
selectPositions "    O OXX" = pure [A3]
selectPositions "    OX X " = pure [A3, C1, C3]
selectPositions "    OX XO" = pure [A1]
selectPositions "    OXOX " = pure [A3]
selectPositions "    OXX  " = pure [A2, A3, C2, C3]
selectPositions "    OXX O" = pure [A1]
selectPositions "    OXXO " = pure [A2]
selectPositions "    X    " = pure [A1, A2, A3, B1, B3, C1, C2, C3]
selectPositions "    X   O" = pure [B3, C2]
selectPositions "    X  O " = pure [A2]
selectPositions "    X  OX" = pure [A1]
selectPositions "    X  XO" = pure [A2]
selectPositions "    X O X" = pure [A1]
selectPositions "    XO X " = pure [A2]
selectPositions "    XOOX " = pure [A2]
selectPositions "    XOX  " = pure [A3]
selectPositions "    XXO  " = pure [B1]
selectPositions "    XXOO " = pure [C3]
selectPositions "   O X   " = pure [A1, A2, A3, B2, C1, C2, C3]
selectPositions "   O X  X" = pure [A3]
selectPositions "   O X X " = pure [C3]
selectPositions "   O X XO" = pure [A1, A2, B2, C1]
selectPositions "   O XX  " = pure [C3]
selectPositions "   O XX O" = pure [A1, A2, A3, B2, C2]
selectPositions "   O XXO " = pure [A1, A2, A3, B2, C3]
selectPositions "   O XXOX" = pure [A3]
selectPositions "   O XXXO" = pure [B2]
selectPositions "   OOX X " = pure [C3]
selectPositions "   OOX XX" = pure [A3, C1]
selectPositions "   OOXOXX" = pure [A3]
selectPositions "   OOXX  " = pure [C3]
selectPositions "   OOXX X" = pure [A3]
selectPositions "   OOXXOX" = pure [A3]
selectPositions "   OOXXXO" = pure [A1]
selectPositions "   OXX   " = pure [A1, A3, C1, C3]
selectPositions "   OXX  O" = pure [A1, A2, C1, C2]
selectPositions "   OXX O " = pure [C3]
selectPositions "   OXX OX" = pure [A1, A3]
selectPositions "   OXX XO" = pure [A2]
selectPositions "   OXXO  " = pure [A1]
selectPositions "   OXXX O" = pure [A3]
selectPositions "   OXXXO " = pure [A3]
selectPositions "   X X  O" = pure [B2]
selectPositions "   X X O " = pure [B2]
selectPositions "   XOX   " = pure [A1, A2, A3, C1, C2, C3]
selectPositions "   XOX  O" = pure [A1]
selectPositions "   XOX O " = pure [A2]
selectPositions "  O   OXX" = pure [B2]
selectPositions "  O   X  " = pure [A1, A2, B2, B3, C3]
selectPositions "  O   XOX" = pure [A1]
selectPositions "  O   XX " = pure [C3]
selectPositions "  O   XXO" = pure [B3]
selectPositions "  O  XX  " = pure [A1, B1, B2]
selectPositions "  O  XX O" = pure [B1]
selectPositions "  O  XXO " = pure [B1]
selectPositions "  O  XXOX" = pure [A1, A2, B1, B2]
selectPositions "  O  XXXO" = pure [A1, A2, B1, B2]
selectPositions "  O OXX  " = pure [A1, A2, C2, C3]
selectPositions "  O OXX X" = pure [C2]
selectPositions "  O OXXOX" = pure [A2]
selectPositions "  O OXXX " = pure [C3]
selectPositions "  O OXXXO" = pure [A1]
selectPositions "  O X OXX" = pure [A1, A2]
selectPositions "  O X X  " = pure [A1, A2, B3, C3]
selectPositions "  OO X  X" = pure [A1, C1]
selectPositions "  OO X X " = pure [A1, A2, B2, C1]
selectPositions "  OO X XX" = pure [C1]
selectPositions "  OO XOXX" = pure [A1]
selectPositions "  OO XX  " = pure [A1, A2, B2, C2, C3]
selectPositions "  OO XX X" = pure [C2]
selectPositions "  OO XXOX" = pure [A1, A2, B2]
selectPositions "  OO XXX " = pure [C3]
selectPositions "  OO XXXO" = pure [A1, A2, B2]
selectPositions "  OOOXX X" = pure [C2]
selectPositions "  OOXX  X" = pure [A1]
selectPositions "  OOXX OX" = pure [A1, A2, C1]
selectPositions "  OOXX X " = pure [A2]
selectPositions "  OOXXO X" = pure [A1]
selectPositions "  OOXXX  " = pure [A1]
selectPositions "  OOXXX O" = pure [A1, A2, C2]
selectPositions "  OOXXXO " = pure [A1, A2, C3]
selectPositions "  OOXXXOX" = pure [A1]
selectPositions "  OOXXXXO" = pure [A2]
selectPositions "  OX    X" = pure [A1, B2]
selectPositions "  OX   OX" = pure [B2]
selectPositions "  OX   X " = pure [A1, A2, B2, B3, C1, C3]
selectPositions "  OX   XO" = pure [B3]
selectPositions "  OX  O X" = pure [B2]
selectPositions "  OX O  X" = pure [A1, C1]
selectPositions "  OX OX X" = pure [A1, C2]
selectPositions "  OX X XO" = pure [B2]
selectPositions "  OX XXO " = pure [A1]
selectPositions "  OXO   X" = pure [C1]
selectPositions "  OXO  X " = pure [C1]
selectPositions "  OXO X X" = pure [A1, C2]
selectPositions "  OXO XOX" = pure [A1]
selectPositions "  OXO XX " = pure [A1, C3]
selectPositions "  OXO XXO" = pure [A1]
selectPositions "  OXOX X " = pure [C1]
selectPositions "  OXOX XO" = pure [A1, C1]
selectPositions "  OXOXX  " = pure [A1]
selectPositions "  OXOXXO " = pure [A1]
selectPositions "  OXOXXOX" = pure [A2]
selectPositions "  OXOXXXO" = pure [A1]
selectPositions "  OXX  OX" = pure [A1, B3]
selectPositions "  OXX O X" = pure [A1, B3]
selectPositions "  OXXO  X" = pure [A1]
selectPositions "  OXXO OX" = pure [A1]
selectPositions "  OXXOO X" = pure [A1]
selectPositions "  X  OXXO" = pure [B2]
selectPositions "  X O X  " = pure [A2, B1, B3, C2]
selectPositions "  X O XO " = pure [A2]
selectPositions "  X O XOX" = pure [A2]
selectPositions "  X OOXX " = pure [C3]
selectPositions "  X OOXXO" = pure [A1, B1]
selectPositions "  X OOXXO" = pure [A1]
selectPositions "  XO   X " = pure [B2, C1]
selectPositions "  XO   XO" = pure [A2]
selectPositions "  XO  OX " = pure [A1]
selectPositions "  XO  OXX" = pure [B3]
selectPositions "  XO  XXO" = pure [B2]
selectPositions "  XO OOXX" = pure [B2]
selectPositions "  XO X XO" = pure [A1, A2, B2, C1]
selectPositions "  XO XOX " = pure [C3]
selectPositions "  XO XOXO" = pure [A1]
selectPositions "  XO XX O" = pure [B2]
selectPositions "  XO XXO " = pure [C3]
selectPositions "  XO XXOO" = pure [B2]
selectPositions "  XOO  X " = pure [B3]
selectPositions "  XOO XX " = pure [B3]
selectPositions "  XOO XXO" = pure [A1, B3]
selectPositions "  XOOX X " = pure [C3]
selectPositions "  XOOX XO" = pure [A1]
selectPositions "  XOOXX  " = pure [C3]
selectPositions "  XOOXX O" = pure [A1]
selectPositions "  XOOXXXO" = pure [B3]
selectPositions "  XOXOOXX" = pure [A2]
selectPositions "  XOXXO O" = pure [C2]
selectPositions "  XOXXOXO" = pure [A2]
selectPositions "  XOXXX O" = pure [A1, A2, C2]
selectPositions "  XX  O O" = pure [C2]
selectPositions "  XX  OOX" = pure [B3]
selectPositions "  XX  OXO" = pure [A2, B2]
selectPositions "  XX O O " = pure [A1, C1]
selectPositions "  XX OO X" = pure [A1, A2, B2]
selectPositions "  XX OOOX" = pure [A1]
selectPositions "  XX OOX " = pure [A1, A2, B2]
selectPositions "  XX OOXO" = pure [A2]
selectPositions "  XX OX O" = pure [A1]
selectPositions "  XX XOO " = pure [B2, C1]
selectPositions "  XXO O X" = pure [B3]
selectPositions "  XXO OX " = pure [A1, A2, B3, C3]
selectPositions "  XXO OXO" = pure [A1]
selectPositions "  XXOO X " = pure [A1, C1]
selectPositions "  XXOO XO" = pure [A1]
selectPositions "  XXOOO X" = pure [A1, A2, C2]
selectPositions "  XXOOOX " = pure [A1, A2, C3]
selectPositions "  XXOOOXX" = pure [A1, A2]
selectPositions "  XXOOX O" = pure [A1]
selectPositions "  XXOOXXO" = pure [A1]
selectPositions "  XXOXO O" = pure [A1, C2]
selectPositions "  XXOXOXO" = pure [A1]
selectPositions "  XXXOO O" = pure [C2]
selectPositions "  XXXOOXO" = pure [A2]
selectPositions " O O X X " = pure [C3]
selectPositions " O O XX X" = pure [A3]
selectPositions " O OXX XO" = pure [A1, A3, C1]
selectPositions " O OXXOX " = pure [A1]
selectPositions " O OXXOXX" = pure [A1, A3]
selectPositions " O OXXXXO" = pure [A3]
selectPositions " O X X XO" = pure [B2]
selectPositions " O X XO X" = pure [B2]
selectPositions " O XOX X " = pure [A1, A3, C1, C3]
selectPositions " O XOX XO" = pure [A1]
selectPositions " O XOXO X" = pure [A3, C2]
selectPositions " O XOXOXX" = pure [A1, A3]
selectPositions " OOX  OXX" = pure [A1]
selectPositions " OOX XXXO" = pure [A1]
selectPositions " OOXOX X " = pure [A1]
selectPositions " OXOXX XO" = pure [A1, C1]
selectPositions " OXX   XO" = pure [B2, C1]
selectPositions " OXX  OXO" = pure [A1, B2, B3]
selectPositions " OXX O X " = pure [A1, B2, C1, C3]
selectPositions " OXX O XO" = pure [C1]
selectPositions " OXX OO X" = pure [A1, B2, C2]
selectPositions " OXX OOX " = pure [A1, B2, C3]
selectPositions " OXX OOXX" = pure [A1, B2]
selectPositions " OXX OXXO" = pure [B2]
selectPositions " OXX XO O" = pure [B2]
selectPositions " OXX XOXO" = pure [B2]
selectPositions " OXXO  XO" = pure [A1]
selectPositions " OXXO OXX" = pure [B3]
selectPositions " OXXOO X " = pure [C1]
selectPositions " OXXOOXX " = pure [A1, C3]
selectPositions " OXXOX XO" = pure [A1, C1]
selectPositions " OXXOXOX " = pure [C3]
selectPositions " OXXX O O" = pure [B2]
selectPositions " OXXX OXO" = pure [B3]
selectPositions " OXXXO XO" = pure [C1]
selectPositions " OXXXOO X" = pure [A1]
selectPositions " OXXXOOX " = pure [A1, C3]
selectPositions " OXXXOOX " = pure [A1, C3]
selectPositions " X  OXXOO" = pure [A1]
selectPositions " X XOX O " = pure [A1, A3, C1, C3]
selectPositions " XOX  OOX" = pure [B2]
selectPositions " XOXO XOX" = pure [A1]
selectPositions " XOXOOX X" = pure [A1, C2]
selectPositions " XOXOOX X" = pure [A1, C2]
selectPositions " XOXX OOX" = pure [A1, B3]
selectPositions "O O  XXXO" = pure [A2, B2]
selectPositions "O O X OXX" = pure [A2, B1]
selectPositions "O OOXXX O" = pure [A2]
selectPositions "O OXOXX X" = pure [A2]
selectPositions "O XO XXXO" = pure [B2]
selectPositions "O XX  O X" = pure [B3]
selectPositions "O XX OO X" = pure [A2, B2, C2]
selectPositions "O XX OOXX" = pure [A2, B2]
selectPositions "O XX OXOX" = pure [B2]
selectPositions "O XX OXXO" = pure [B2]
selectPositions "O XXO OXX" = pure [B3]
selectPositions "O XXO XOX" = pure [A2]
selectPositions "O XXOOX X" = pure [C2]
selectPositions "O XXXOO X" = pure [A2, C2]
selectPositions "OOXX  OXX" = pure [B3]
selectPositions "OOXX OX X" = pure [B2, C2]
selectPositions "OOXXO X X" = pure [C2]

selectPositions s           = Except.throwError ("unknown board <\n" <> List.intercalate "\n" (Split.chunksOf 3 s) <> "\n>")
