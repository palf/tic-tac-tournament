{-# LANGUAGE DeriveGeneric #-}

module Options
  ( AgentName (..)
  , Options (..)
  , readOptions
  ) where

import qualified Options.Applicative as Opt

import           Data.String         (IsString, fromString)
import           GHC.Generics        (Generic)
import           Options.Applicative ((<**>))


data AgentName = Learner01 | Learner02 | Random | Minimax | Perfect deriving
  ( Eq
  , Generic
  , Show
  )

instance IsString AgentName where
  fromString "random"    = Random
  fromString "learner01" = Learner01
  fromString "learner02" = Learner02
  fromString "perfect"   = Perfect
  fromString "minimax"   = Minimax
  fromString s           = error ("bad agent name: " <> s)


data Options
  = Options
    { total  :: Int
    , agentX :: AgentName
    , agentO :: AgentName
    }
  deriving (Eq, Generic, Show)


options :: Opt.Parser Options
options = Options
      <$> totalOption
      <*> agentXOption
      <*> agentOOption

  where
    totalOption =  Opt.option Opt.auto
      (  Opt.long "total"
      <> Opt.short 't'
      <> Opt.help "How many games to run"
      <> Opt.showDefault
      <> Opt.value 1
      <> Opt.metavar "TOTAL" )

    agentXOption :: Opt.Parser AgentName
    agentXOption = Opt.strOption
      (  Opt.short 'X'
      <> Opt.help "which agent to use for X"
      <> Opt.showDefault
      <> Opt.value Random
      <> Opt.metavar "PLAYER_X" )

    agentOOption :: Opt.Parser AgentName
    agentOOption = Opt.strOption
      (  Opt.short 'O'
      <> Opt.help "which agent to use for O"
      <> Opt.showDefault
      <> Opt.value Random
      <> Opt.metavar "PLAYER_O" )


opts :: Opt.ParserInfo Options
opts = Opt.info (options <**> Opt.helper)
  ( Opt.fullDesc
  <> Opt.progDesc "Runs tic-tac-toe games with selected agents"
  <> Opt.header "Tic Tac Tournament" )


readOptions :: IO Options
readOptions = Opt.execParser opts
