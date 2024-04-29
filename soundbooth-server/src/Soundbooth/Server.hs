{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Soundbooth.Server (
  Cues (..),
  Cue (..),
  SoundName (..),
  Request (..),
  Response (..),
  Event (..),
  optionsP,
  Options (..),
  defaultMainWith,
  defaultMain,
) where

import Control.Applicative ((<**>))
import Control.Concurrent.STM (TBQueue, newTBQueueIO)
import Control.Foldl qualified as L
import Control.Lens (ifolded, withIndex)
import Control.Monad (forM_, when)
import Data.Aeson qualified as J
import Data.Bifoldable (bimapM_)
import Data.ByteString.Char8 qualified as BS8
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Data.Yaml qualified as Y
import DeferredFolds.UnfoldlM qualified as U
import Effectful
import Effectful.Audio
import Effectful.Concurrent (threadDelay)
import Effectful.Concurrent.Async
import Effectful.Concurrent.STM (atomically, readTBQueue, writeTBQueue)
import Effectful.Console.ByteString (Console, runConsole)
import Effectful.Console.ByteString qualified as Console
import Effectful.Error.Static (Error, runErrorNoCallStackWith)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.FileSystem.IO (stderr)
import Effectful.FileSystem.IO.ByteString (hPutStrLn)
import Effectful.Reader.Static (Reader, ask, asks, runReader)
import Focus qualified
import GHC.Generics
import Options.Applicative qualified as Opts
import Soundbooth.Common.Types
import StmContainers.Map qualified as TMap
import Streaming.Prelude qualified as S

newtype Cues = Cues {cues :: V.Vector Cue}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON)

data Cue = Cue {name :: !SoundName, path :: !Text}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON)

data Options = Options
  { cueFile :: !FilePath
  , audioOpts :: !AudioOptions
  , stopDetectIntervalInSec :: !Double
  }
  deriving (Show, Eq, Ord, Generic)

newtype ServerEnv = ServerEnv {samples :: HashMap SoundName Sample}
  deriving (Generic)

data ServerState = ServerState
  { playing :: TMap.Map SoundName Sound
  , evtQ :: TBQueue Event
  , respQ :: TBQueue Response
  , reqQ :: TBQueue Request
  }
  deriving (Generic)

optionsP :: Opts.ParserInfo Options
optionsP = Opts.info (parser <**> Opts.helper) (Opts.fullDesc <> Opts.progDesc "Audio cue server")
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
      pure Options {audioOpts = AudioOptions {..}, ..}

defaultMainWith :: Options -> IO ()
defaultMainWith opts@Options {..} = do
  evtQ <- newTBQueueIO 16
  respQ <- newTBQueueIO 16
  reqQ <- newTBQueueIO 16
  cs <- Y.decodeFileThrow @_ @Cues cueFile
  playing <- TMap.newIO
  runEff $ runAudio audioOpts $ do
    samples <- HM.fromList <$> mapM (\Cue {..} -> (name,) <$> sampleFromFile (T.unpack path) 1.0) (V.toList cs.cues)
    runConsole $ runConcurrent $ runReader opts $ runReader ServerEnv {..} do
      runFileSystem (send reqQ)
        `race_` S.print (S.repeatM (atomically (readTBQueue respQ)))
        `race_` S.print (S.repeatM (atomically (readTBQueue evtQ)))
        `race_` runReader ServerState {..} (mainLoop `race_` checkIfStopped)

checkIfStopped ::
  (Reader ServerState :> es, Concurrent :> es, Reader Options :> es, Audio :> es) =>
  Eff es ()
checkIfStopped = go mempty
  where
    go playing = do
      threadDelay . floor . (1_000_000 *)
        =<< asks @Options (.stopDetectIntervalInSec)
      sst <- asks @ServerState (.playing)
      (nowPlaying, inactives) <-
        L.foldOverM
          (ifolded . withIndex)
          ( L.premapM
              (traverse $ liftA2 (,) <$> pure <*> isActive)
              ( L.generalize $
                  (,)
                    <$> L.prefilter
                      (snd . snd)
                      (L.premap (fmap fst) L.map)
                    <*> L.prefilter
                      (not . snd . snd)
                      (L.premap fst L.set)
              )
          )
          =<< atomically do
            U.foldM (L.generalize L.map) $ TMap.unfoldlM sst
      atomically $ forM_ inactives $ (`TMap.delete` sst)
      let !stopped = inactives `Set.union` (Map.keysSet playing Set.\\ Map.keysSet nowPlaying)
      forM_ (NE.nonEmpty $ Set.toList stopped) $ \stops -> do
        evts <- asks @ServerState (.evtQ)
        atomically $ writeTBQueue evts $ Stopped stops
      go nowPlaying

mainLoop ::
  ( Concurrent :> es
  , Reader ServerState :> es
  , Reader ServerEnv :> es
  , Console :> es
  , Audio :> es
  ) =>
  Eff es ()
mainLoop = do
  ServerState {..} <- ask
  S.repeatM (atomically $ readTBQueue reqQ)
    & S.mapM processReq
    & S.mapM_
      ( atomically
          . bimapM_
            (mapM_ $ writeTBQueue respQ)
            (mapM_ $ writeTBQueue evtQ)
      )

processReq ::
  ( Reader ServerState :> es
  , Concurrent :> es
  , Audio :> es
  , Reader ServerEnv :> es
  , Console :> es
  ) =>
  Request ->
  Eff es ([Response], [Event])
processReq (Play sn) = withSample sn \sample -> do
  playing <- asks @ServerState (.playing)
  mapM_ soundStop
    =<< atomically (TMap.focus Focus.lookupAndDelete sn playing)
  s <- play sample 1.0 1.0 0.0 1.0
  atomically $ TMap.insert s sn playing
  pure ([Ok], [Playing $ sn NE.:| []])
processReq (Stop sn) = do
  playing <- asks @ServerState (.playing)
  stopped <-
    fmap or . mapM stop =<< atomically (TMap.lookup sn playing)
  Console.putStrLn $ "stopped?: " <> BS8.pack (show stopped)
  when stopped $
    atomically $
      TMap.delete sn playing
  pure ([Ok], [Stopped $ NE.singleton sn | stopped])
processReq StopAll = mempty <$ stopAll

withSample ::
  (Reader ServerEnv :> es) =>
  SoundName ->
  (Sample -> Eff (Error Response ': es) ([Response], [Event])) ->
  Eff es ([Response], [Event])
withSample sn k = do
  smpls <- asks @ServerEnv (.samples)
  runErrorNoCallStackWith (\rsp -> pure ([rsp], [])) do
    maybe (pure ([UnknownSound sn], [])) k $ HM.lookup sn smpls

send :: (FileSystem :> es, Console :> es, Concurrent :> es) => TBQueue Request -> Eff es ()
send reqs =
  S.repeatM Console.getLine
    & S.map J.eitherDecodeStrict
    & S.mapM_ (either (hPutStrLn stderr . TE.encodeUtf8 . T.pack . ("Invalid input: " <>)) (atomically . writeTBQueue reqs))

defaultMain :: IO ()
defaultMain = defaultMainWith =<< Opts.execParser optionsP
