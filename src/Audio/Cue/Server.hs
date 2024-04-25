{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Audio.Cue.Server (
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
import Control.DeepSeq (NFData)
import Control.Exception.Safe
import Control.Foldl qualified as L
import Control.Monad (forM_, unless, when)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as J
import Data.Bifoldable (bimapM_)
import Data.ByteString.Char8 qualified as BS8
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Data.String (IsString)
import Data.Text (Text)
import Data.Vector qualified as V
import Data.Yaml qualified as Y
import DeferredFolds.UnfoldlM qualified as U
import Effectful
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
import GHC.Generics (Generic)
import Options.Applicative qualified as Opts
import Sound.ProteaAudio
import StmContainers.Map qualified as TMap
import Streaming.Prelude qualified as S

newtype Cues = Cues {cues :: V.Vector Cue}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype SoundName = SoundName Text
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, ToJSON, IsString, Hashable, NFData)

data Cue = Cue {name :: !SoundName, path :: !FilePath}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Request
  = Play !SoundName
  | Stop !SoundName
  | StopAll
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Response = Ok | UnknownSound !SoundName | Error !Text
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Event
  = Playing !(NonEmpty SoundName)
  | Stopped !(NonEmpty SoundName)
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Options = Options
  { cueFile :: !FilePath
  , audioOpts :: !AudioOptions
  , stopDetectIntervalInSec :: !Double
  }
  deriving (Show, Eq, Ord, Generic)

data AudioOptions = AudioOptions
  { numTracks :: !Int
  , sampleFreq :: !Int
  , bucketSize :: !Int
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
defaultMainWith opts@Options {..} = withAudio audioOpts do
  evtQ <- newTBQueueIO 16
  respQ <- newTBQueueIO 16
  reqQ <- newTBQueueIO 16
  cs <- Y.decodeFileThrow @_ @Cues cueFile
  samples <- HM.fromList <$> mapM (\Cue {..} -> (name,) <$> sampleFromFile path 1.0) (V.toList cs.cues)
  playing <- TMap.newIO
  runEff $ runConsole $ runConcurrent $ runReader opts $ runReader ServerEnv {..} do
    runFileSystem (send reqQ)
      `race_` S.print (S.repeatM (atomically (readTBQueue respQ)))
      `race_` S.print (S.repeatM (atomically (readTBQueue evtQ)))
      `race_` runReader ServerState {..} (mainLoop `race_` checkIfStopped)

checkIfStopped ::
  (Reader ServerState :> es, Concurrent :> es, Reader Options :> es) =>
  Eff es ()
checkIfStopped = go Set.empty
  where
    go playing = do
      threadDelay . floor . (1_000_000 *)
        =<< asks @Options (.stopDetectIntervalInSec)
      sst <- asks @ServerState (.playing)
      nowPlaying <- atomically do
        U.foldM (L.generalize $ L.premap fst L.set) $ TMap.unfoldlM sst
      let !stopped = playing Set.\\ nowPlaying
      forM_ (NE.nonEmpty $ Set.toList stopped) $ \stops -> do
        evts <- asks @ServerState (.evtQ)
        atomically $ writeTBQueue evts $ Stopped stops
      go nowPlaying

mainLoop ::
  ( Concurrent :> es
  , Reader ServerState :> es
  , IOE :> es
  , Reader ServerEnv :> es
  , Console :> es
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
  , IOE :> es
  , Reader ServerEnv :> es
  , Console :> es
  ) =>
  Request ->
  Eff es ([Response], [Event])
processReq (Play sn) = withSample sn \sample -> do
  playing <- asks @ServerState (.playing)
  mapM_ (liftIO . soundStop)
    =<< atomically (TMap.focus Focus.lookupAndDelete sn playing)
  s <- liftIO $ soundPlay sample 1.0 1.0 0.0 1.0
  atomically $ TMap.insert s sn playing
  pure ([Ok], [Playing $ sn NE.:| []])
processReq (Stop sn) = do
  playing <- asks @ServerState (.playing)
  stopped <-
    fmap or . mapM (liftIO . soundStop)
      =<< atomically (TMap.lookup sn playing)
  Console.putStrLn $ "stopped?: " <> BS8.pack (show stopped)
  when stopped $
    atomically $
      TMap.delete sn playing
  pure ([Ok], [Stopped $ NE.singleton sn | stopped])
processReq StopAll = mempty <$ liftIO soundStopAll

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
    & S.mapM_ (either (hPutStrLn stderr . BS8.pack . ("Invalid input: " <>)) (atomically . writeTBQueue reqs))

withAudio :: AudioOptions -> IO () -> IO ()
withAudio aopts = bracket_ (initAudio' aopts) finishAudio

initAudio' :: AudioOptions -> IO ()
initAudio' aopts =
  (`unless` throwString "Audio initialisation failed!")
    =<< initAudio aopts.numTracks aopts.sampleFreq aopts.bucketSize

defaultMain :: IO ()
defaultMain = defaultMainWith =<< Opts.execParser optionsP
