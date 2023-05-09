{-# LANGUAGE DeriveGeneric #-}

module Options
  ( Options (..)
  , readOptions
  ) where

import qualified Options.Applicative as Opt

import           GHC.Generics        (Generic)
import           Options.Applicative ((<**>))


newtype Options
  = Options { total :: Int }
  deriving (Eq, Generic, Show)


options :: Opt.Parser Options
options = Options <$> totalOption

  where
    totalOption =  Opt.option Opt.auto
      (  Opt.long "total"
      <> Opt.short 't'
      <> Opt.help "How many games to run"
      <> Opt.showDefault
      <> Opt.value 1
      <> Opt.metavar "TOTAL" )


opts :: Opt.ParserInfo Options
opts = Opt.info (options <**> Opt.helper)
  ( Opt.fullDesc
  <> Opt.progDesc "trains the learner agent"
  <> Opt.header "Learner trainer" )


readOptions :: IO Options
readOptions = Opt.execParser opts
