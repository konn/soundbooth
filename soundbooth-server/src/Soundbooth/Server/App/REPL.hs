{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Soundbooth.Server.App.REPL (
  defaultMainWith,
  defaultMain,
  optionsP,
  Options (..),
) where

import Control.Applicative ((<**>))
import Data.Aeson qualified as J
import Data.Function ((&))
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Yaml qualified as Y
import Effectful
import Effectful.Audio
import Effectful.Concurrent.Async
import Effectful.Concurrent.STM (atomically)
import Effectful.Console.ByteString (Console, runConsole)
import Effectful.Console.ByteString qualified as Console
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.FileSystem.IO (stderr)
import Effectful.FileSystem.IO.ByteString (hPutStrLn)
import GHC.Generics
import Options.Applicative qualified as Opts
import Soundbooth.Common.Types
import Soundbooth.Server.Player
import Streaming.Prelude qualified as S

data Options = Options
  { cueFile :: !FilePath
  , playerOpts :: !PlayerOptions
  }
  deriving (Show, Eq, Ord, Generic)

optionsP :: Opts.ParserInfo Options
optionsP = Opts.info (parser <**> Opts.helper) (Opts.fullDesc <> Opts.progDesc "A CLI interface for Soundbooth audio cue system")
  where
    parser = do
      cueFile <-
        Opts.strArgument $
          Opts.metavar "FILE"
            <> Opts.help "Path to the cue file"
            <> Opts.value "cues.yaml"
            <> Opts.showDefault
      numTracks <-
        Opts.option
          Opts.auto
          ( Opts.long "num-tracks"
              <> Opts.metavar "INT"
              <> Opts.help "Maximum # of tracks to play simultaneously"
              <> Opts.value 3
              <> Opts.showDefault
          )
      sampleFreq <-
        Opts.option
          Opts.auto
          ( Opts.long "freq"
              <> Opts.short 'f'
              <> Opts.metavar "INT"
              <> Opts.help "Sampling rate (22050 for FM, 44100 for CD)"
              <> Opts.value 44100
              <> Opts.showDefault
          )
      bucketSize <-
        Opts.option
          Opts.auto
          ( Opts.long "bucket-size"
              <> Opts.short 'B'
              <> Opts.metavar "INT"
              <> Opts.help "The # of bytes sent to the audio device at once"
              <> Opts.value 512
              <> Opts.showDefault
          )
      stopDetectIntervalInSec <-
        Opts.option Opts.auto $
          Opts.long "stop-interval"
            <> Opts.short 'I'
            <> Opts.metavar "SECS"
            <> Opts.help "Interval in seconds to check if a sound has stopped"
            <> Opts.value 0.5
            <> Opts.showDefault

      pure $
        let audioOptions = AudioOptions {..}
         in Options {playerOpts = PlayerOptions {..}, ..}

defaultMainWith :: Options -> IO ()
defaultMainWith Options {..} = do
  cs <- Y.decodeFileThrow @_ @Cues cueFile
  runEff $ runConsole $ runConcurrent $ do
    qs <- newPlayerQueues 64
    conn <- subscribe qs
    runFileSystem (send (atomically . sendRequest conn))
      `race_` S.print (S.repeatM (atomically (readResponse conn)))
      `race_` S.print (S.repeatM (atomically (readEvent conn)))
      `race_` runPlayer playerOpts qs cs

send :: (FileSystem :> es, Console :> es) => (Request -> Eff es ()) -> Eff es ()
send put =
  S.repeatM Console.getLine
    & S.map J.eitherDecodeStrict
    & S.mapM_ (either (hPutStrLn stderr . TE.encodeUtf8 . T.pack . ("Invalid input: " <>)) put)

defaultMain :: IO ()
defaultMain = defaultMainWith =<< Opts.execParser optionsP
