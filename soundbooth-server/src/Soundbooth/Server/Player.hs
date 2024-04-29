{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Soundbooth.Server.Player (
  Cues (..),
  Cue (..),
  runPlayer,
  SoundName (..),
  Request (..),
  Response (..),
  Event (..),
  PlayerOptions (..),
  newPlayerQueues,
  PlayerQueues (),
  subscribe,
  ClientQueues (),
  readEvent,
  readResponse,
  sendRequest,
) where

import Control.Foldl qualified as L
import Control.Lens (ifolded, withIndex)
import Control.Monad (forM_, when)
import Data.Aeson qualified as J
import Data.Bifoldable (bimapM_)
import Data.Foldable (foldl')
import Data.Function ((&))
import Data.List.NonEmpty qualified as NE
import Data.Map.Ordered qualified as OMap (alter)
import Data.Map.Ordered.Strict (OMap)
import Data.Map.Ordered.Strict qualified as OMap
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import DeferredFolds.UnfoldlM qualified as U
import Effectful
import Effectful.Audio
import Effectful.Concurrent (threadDelay)
import Effectful.Concurrent.Async
import Effectful.Concurrent.STM
import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.Error.Static (Error, runErrorNoCallStackWith)
import Effectful.Reader.Static (Reader, asks, runReader)
import Focus qualified
import GHC.Generics
import Numeric.Natural
import Soundbooth.Common.Types
import StmContainers.Map qualified as TMap
import Streaming.Prelude qualified as S

newtype Cues = Cues {cues :: V.Vector Cue}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON)

data Cue = Cue {name :: !SoundName, path :: !Text}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON)

data PlayerQueues = PlayerQueues
  { evtQ :: TChan Event
  , respQ :: TChan Response
  , reqQ :: TBQueue Request
  }

data ClientQueues = ClientQueues
  { evtQ :: TChan Event
  , respQ :: TChan Response
  , reqQ :: TBQueue Request
  }

runPlayer :: (IOE :> es, Concurrent :> es) => PlayerOptions -> PlayerQueues -> Cues -> Eff es ()
runPlayer opts queues cs = do
  playing <- unsafeEff_ TMap.newIO
  runAudio opts.audioOptions $ do
    samples <- OMap.fromList <$> mapM (\Cue {..} -> (name,) <$> sampleFromFile (T.unpack path) 1.0) (V.toList cs.cues)
    runReader opts $ runReader ServerEnv {..} do
      runReader ServerState {..} (mainLoop `race_` checkIfStopped)

data PlayerOptions = PlayerOptions
  { audioOptions :: !AudioOptions
  , stopDetectIntervalInSec :: !Double
  }
  deriving (Show, Eq, Ord, Generic)

newtype PlayerEnv = ServerEnv {samples :: OMap SoundName Sample}
  deriving (Generic)

readEvent :: ClientQueues -> STM Event
readEvent ClientQueues {evtQ} = readTChan evtQ

readResponse :: ClientQueues -> STM Response
readResponse ClientQueues {respQ} = readTChan respQ

sendRequest :: ClientQueues -> Request -> STM ()
sendRequest ClientQueues {reqQ} = writeTBQueue reqQ

subscribe :: (Concurrent :> es) => PlayerQueues -> Eff es ClientQueues
subscribe qs = do
  evtQ <- atomically $ dupTChan qs.evtQ
  respQ <- atomically $ dupTChan qs.respQ
  pure ClientQueues {reqQ = qs.reqQ, ..}

data PlayerState = ServerState
  { playing :: TMap.Map SoundName Sound
  , queues :: PlayerQueues
  }
  deriving (Generic)

checkIfStopped ::
  (Reader PlayerState :> es, Concurrent :> es, Reader PlayerOptions :> es, Audio :> es) =>
  Eff es ()
checkIfStopped = go mempty
  where
    go playing = do
      threadDelay . floor . (1_000_000 *)
        =<< asks @PlayerOptions (.stopDetectIntervalInSec)
      sst <- asks @PlayerState (.playing)
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
        evts <- asks @PlayerState (.queues.evtQ)
        atomically $ writeTChan evts $ Stopped stops
      go nowPlaying

mainLoop ::
  ( Concurrent :> es
  , Reader PlayerState :> es
  , Reader PlayerEnv :> es
  , Audio :> es
  ) =>
  Eff es ()
mainLoop = do
  PlayerQueues {..} <- asks @PlayerState (.queues)
  S.repeatM (atomically $ readTBQueue reqQ)
    & S.mapM processReq
    & S.mapM_
      ( atomically
          . bimapM_
            (mapM_ $ writeTChan respQ)
            (mapM_ $ writeTChan evtQ)
      )

newPlayerQueues :: (Concurrent :> es) => Natural -> Eff es PlayerQueues
newPlayerQueues size = do
  evtQ <- newBroadcastTChanIO
  respQ <- newBroadcastTChanIO
  reqQ <- newTBQueueIO size
  pure PlayerQueues {..}

processReq ::
  ( Reader PlayerState :> es
  , Concurrent :> es
  , Audio :> es
  , Reader PlayerEnv :> es
  ) =>
  Request ->
  Eff es ([Response], [Event])
processReq GetPlaylist = do
  sst <- asks @PlayerState (.playing)
  smpls <- asks @PlayerEnv (.samples)
  pls <- atomically $ do
    U.foldM (L.generalize $ L.premap fst L.set) $ TMap.unfoldlM sst
  let pl =
        Playlist $
          V.fromList $
            OMap.assocs $
              foldl' (flip $ OMap.alter (const $ Just Playing)) (Idle <$ smpls) pls
  pure ([Ok], [CurrentPlaylist pl])
processReq (Play sn) = withSample sn \sample -> do
  playing <- asks @PlayerState (.playing)
  mapM_ soundStop
    =<< atomically (TMap.focus Focus.lookupAndDelete sn playing)
  s <- play sample 1.0 1.0 0.0 1.0
  atomically $ TMap.insert s sn playing
  pure ([Ok], [Started $ sn NE.:| []])
processReq (Stop sn) = do
  playing <- asks @PlayerState (.playing)
  stopped <-
    fmap or . mapM stop =<< atomically (TMap.lookup sn playing)
  when stopped $
    atomically $
      TMap.delete sn playing
  pure ([Ok], [Stopped $ NE.singleton sn | stopped])
processReq StopAll = mempty <$ stopAll

withSample ::
  (Reader PlayerEnv :> es) =>
  SoundName ->
  (Sample -> Eff (Error Response ': es) ([Response], [Event])) ->
  Eff es ([Response], [Event])
withSample sn k = do
  smpls <- asks @PlayerEnv (.samples)
  runErrorNoCallStackWith (\rsp -> pure ([rsp], [])) do
    maybe (pure ([UnknownSound sn], [])) k $ OMap.lookup sn smpls
