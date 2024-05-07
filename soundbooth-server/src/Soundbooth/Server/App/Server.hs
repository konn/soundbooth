{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Soundbooth.Server.App.Server (
  optionsP,
  Options (..),
  defaultMainWith,
  defaultMain,
) where

import Control.Applicative ((<**>))
import Control.Monad (forever)
import Data.Function ((&))
import Data.Text qualified as T
import Data.Yaml qualified as Y
import Effectful (Eff, runEff)
import Effectful qualified as Eff
import Effectful.Audio
import Effectful.Concurrent (threadDelay)
import Effectful.Concurrent.Async
import Effectful.Concurrent.STM (atomically)
import Effectful.Console.ByteString (runConsole)
import Effectful.Log
import Effectful.Log.Extra (runStdErrLogger)
import Effectful.Reader.Static (Reader, ask, runReader)
import Effectful.Servant
import Effectful.WebSockets qualified as WS
import GHC.Generics
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp qualified as Warp
import Options.Applicative qualified as Opts
import Servant
import Servant.API.WebSocket qualified as WS
import Servant.Server.Generic
import Soundbooth.Common.Types
import Soundbooth.Server.Cueing
import Soundbooth.Server.Orphans ()
import Streaming.Prelude qualified as S
import WaiAppStatic.Types (unsafeToPiece)

type (∈) = (Eff.:>)

data Options = Options
  { cueFile :: !FilePath
  , playerOpts :: !PlayerOptions
  , backendOpts :: !BackendOptions
  }
  deriving (Show, Eq, Ord, Generic)

optionsP :: Opts.ParserInfo Options
optionsP = Opts.info (parser <**> Opts.helper) (Opts.fullDesc <> Opts.progDesc "A web-based server interface for Soundbooth audio cue system")
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

      backendOpts <- backendOptsP
      pure $
        let audioOptions = AudioOptions {..}
         in Options {playerOpts = PlayerOptions {..}, ..}

backendOptsP :: Opts.Parser BackendOptions
backendOptsP = do
  port <-
    Opts.option Opts.auto $
      Opts.long "port"
        <> Opts.short 'P'
        <> Opts.metavar "PORT"
        <> Opts.help "Port to listen on"
        <> Opts.value 41010
        <> Opts.showDefault
  staticDir <-
    Opts.strOption $
      Opts.long "serve"
        <> Opts.short 'i'
        <> Opts.metavar "DIR"
        <> Opts.help "Directory to serve static files from"
        <> Opts.value "_build"
        <> Opts.showDefault
  pure BackendOptions {..}

data BackendOptions = BackendOptions
  { staticDir :: !FilePath
  , port :: !Int
  }
  deriving (Show, Eq, Ord, Generic)

type TheAPI = ToServantApi APIRoutes

data APIRoutes mode = APIRoutes
  { websocket :: mode :- "ws" :> WS.WebSocket
  , static :: mode :- Raw
  }
  deriving (Generic)

defaultMain :: IO ()
defaultMain = defaultMainWith =<< Opts.execParser optionsP

defaultMainWith :: Options -> IO ()
defaultMainWith Options {..} = do
  cs <- Y.decodeFileThrow @_ @Config cueFile
  runEff $
    runStdErrLogger "backend" LogTrace $
      runConsole $
        runConcurrent $
          runReader backendOpts $ do
            (qs, pqs) <- newCueingQueues
            runReader qs $
              runReader pqs $ do
                logInfo_ $ "Server is listening: http://localhost:" <> tshow backendOpts.port
                runCueingServer playerOpts qs pqs cs
                  `race_` WS.runWebSocketsIO do
                    runWarpServerSettings @TheAPI
                      (Warp.defaultSettings & Warp.setPort backendOpts.port)
                      (genericServerT @APIRoutes $ theServer backendOpts)

tshow :: (Show a) => a -> T.Text
tshow = T.pack . show

theServer ::
  ( Reader CueingQueues ∈ es
  , Concurrent ∈ es
  , WS.WebSockets ∈ es
  , Log Eff.:> es
  ) =>
  BackendOptions ->
  APIRoutes (AsServerT (Eff es))
theServer BackendOptions {..} = do
  APIRoutes
    { static = serveStatics staticDir
    , websocket = handleWs
    }

handleWs ::
  ( Concurrent ∈ es
  , Reader CueingQueues ∈ es
  , WS.WebSockets ∈ es
  , Log Eff.:> es
  ) =>
  WS.Connection ->
  Eff es ()
handleWs conn = do
  qs <- subscribeCue =<< ask @CueingQueues
  atomically $ mapM_ (sendCueRequest qs) [PlayerRequest GetPlaylist, GetCueState]
  runReader qs $
    sendResp
      `race_` sendEvt
      `race_` recvr
      `race_` forever do
        threadDelay $ 30_000_000
        WS.sendPing conn $ PlayerEvent KeepAlive
  where
    sendEvt = do
      qs <- ask @CueingClientQueues
      S.repeatM (atomically $ readCueEvent qs)
        & S.chain (logInfo "Sending event to conn: ")
        & S.mapM_ (WS.sendTextData conn)
    sendResp = do
      qs <- ask @CueingClientQueues
      S.repeatM (atomically (readCueResponse qs))
        & S.chain (logInfo "Sending resp to conn: ")
        & S.mapM_ (WS.sendTextData conn)
    recvr = do
      qs <- ask @CueingClientQueues
      S.repeatM (WS.receiveData conn)
        & S.chain (logInfo "Request: ")
        & S.mapM_ (atomically . sendCueRequest qs)

serveStatics :: FilePath -> Tagged (Eff es) Application
serveStatics src =
  Tagged $
    staticApp $
      (defaultWebAppSettings src)
        { ssRedirectToIndex = True
        , ssIndices = map unsafeToPiece ["index.html", "index.htm"]
        }
