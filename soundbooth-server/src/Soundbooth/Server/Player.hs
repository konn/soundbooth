{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Soundbooth.Server.Player (
  Config (..),
  Music (..),
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
  sendEvent,
) where

import Control.Foldl qualified as L
import Control.Lens (ifolded, withIndex)
import Control.Monad (forM_, forever, unless, when)
import Control.Monad.Trans (lift)
import Data.Foldable (foldl')
import Data.Function ((&))
import Data.Functor (void)
import Data.Functor.Of (Of)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Ordered qualified as OMap (alter)
import Data.Map.Ordered.Strict (OMap)
import Data.Map.Ordered.Strict qualified as OMap
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Ratio ((%))
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Vector qualified as V
import DeferredFolds.UnfoldlM qualified as U
import Effectful
import Effectful.Audio
import Effectful.Concurrent (threadDelay)
import Effectful.Concurrent.Async
import Effectful.Concurrent.STM
import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.Reader.Static (Reader, asks, runReader)
import Focus qualified
import GHC.Generics
import Soundbooth.Common.Types
import Soundbooth.Server.Types
import StmContainers.Map qualified as TMap
import Streaming qualified as S
import Streaming.Prelude qualified as S

runPlayer :: (IOE :> es, Concurrent :> es) => PlayerOptions -> PlayerQueues -> Config -> Eff es ()
runPlayer opts queues cs = do
  playing <- unsafeEff_ TMap.newIO
  runAudio opts.audioOptions $ do
    samples <- OMap.fromList <$> mapM (\Music {..} -> (name,) <$> sampleFromFile (T.unpack path) 1.0) (V.toList cs.files)
    runReader opts $ runReader ServerEnv {..} do
      runReader ServerState {..} (mainLoop `race_` checkIfStopped)

data PlayerOptions = PlayerOptions
  { audioOptions :: !AudioOptions
  , stopDetectIntervalInSec :: !Double
  }
  deriving (Show, Eq, Ord, Generic)

newtype PlayerEnv = ServerEnv {samples :: OMap SoundName Sample}
  deriving (Generic)

data PlayerState = ServerState
  { playing :: TMap.Map SoundName Sound
  , queues :: PlayerQueues
  }
  deriving (Generic)

checkIfStopped ::
  (Reader PlayerState :> es, Concurrent :> es, Reader PlayerOptions :> es, Audio :> es) =>
  Eff es ()
checkIfStopped = forever do
  threadDelay . floor . (1_000_000 *)
    =<< asks @PlayerOptions (.stopDetectIntervalInSec)
  sst <- asks @PlayerState (.playing)
  inactives <-
    L.foldOverM
      (ifolded . withIndex)
      ( L.prefilterM
          (fmap not . isActive . snd)
          $ L.generalize
          $ L.premap fst L.set
      )
      =<< atomically do
        U.foldM (L.generalize L.map) $ TMap.unfoldlM sst
  atomically $ forM_ inactives $ (`TMap.delete` sst)
  forM_ (NE.nonEmpty $ Set.toList inactives) $ \stops -> do
    evts <- asks @PlayerState (.queues.evtQ)
    atomically $ writeTChan evts $ Finished stops

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
    & flip S.for processReq
    & S.mapM_
      ( atomically
          . either
            (writeTChan respQ)
            (writeTChan evtQ)
      )

processReq ::
  ( Reader PlayerState :> es
  , Concurrent :> es
  , Audio :> es
  , Reader PlayerEnv :> es
  ) =>
  Request ->
  S.Stream (Of (Either Response Event)) (Eff es) ()
processReq GetPlaylist = do
  sst <- lift $ asks @PlayerState (.playing)
  smpls <- lift $ asks @PlayerEnv (.samples)
  pls <- lift $ atomically $ do
    U.foldM (L.generalize $ L.premap fst L.set) $ TMap.unfoldlM sst
  let pl =
        Playlist $
          V.fromList $
            OMap.assocs $
              foldl' (flip $ OMap.alter (const $ Just Playing)) (Idle <$ smpls) pls
  S.yield $ Left Ok
  S.yield $ Right $ CurrentPlaylist pl
processReq (Play sn) = cutIn sn
processReq (Stop sn) = cutOut sn
processReq StopAll = lift stopAll *> S.yield (Left Ok)
processReq (FadeIn dur sn) = fadeIn dur sn
processReq (FadeOut dur sn) = fadeOut dur sn
processReq (CrossFade dur froms tos) = crossFade dur froms tos
processReq GetCues = do
  S.yield $ Left Ok
  S.yield $ Right $ CurrentCues mempty

interpolateStep :: Fading -> Float -> Float -> Int -> Float
interpolateStep Fading {..} start end i =
  case fromMaybe Linear interpolation of
    Quadratic ->
      let a = end - start
          b = -2 * a * end
          c = start
          x = fromRational $ fromIntegral i % fromIntegral steps
       in a * x * x + b * x + c
    Linear -> start + fromIntegral i * (end - start) / fromIntegral steps

fadeIn ::
  ( Reader PlayerEnv :> es
  , Reader PlayerState :> es
  , Audio :> es
  , Concurrent :> es
  ) =>
  Fading ->
  SoundName ->
  S.Stream (Of (Either Response Event)) (Eff es) ()
fadeIn dur sn = withSample sn \sample -> do
  let usec = floor $ 1_000_000 * dur.duration / fromIntegral dur.steps
      level = interpolateStep dur 0.0 1.0
  playing <- lift $ asks @PlayerState (.playing)
  stopped <-
    lift $
      fmap or . mapM stop =<< atomically (TMap.lookup sn playing)
  when stopped $
    lift $
      atomically $
        TMap.delete sn playing
  lift $
    mapM_ soundStop
      =<< atomically (TMap.focus Focus.lookupAndDelete sn playing)
  s <- lift $ play sample 0.0 0.0 0.0 1.0
  S.yield $ Right $ Started $ NE.singleton sn
  lift $ atomically $ TMap.insert s sn playing
  forM_ [1 .. dur.steps] \i -> lift do
    threadDelay usec
    update s False (level i) (level i) 0.0 1.0
  S.yield $ Left Ok

fadeOut ::
  ( Reader PlayerEnv :> es
  , Reader PlayerState :> es
  , Audio :> es
  , Concurrent :> es
  ) =>
  Fading ->
  SoundName ->
  S.Stream (Of (Either Response Event)) (Eff es) ()
fadeOut dur sn = withSample sn \_ -> do
  let usec = floor $ 1_000_000 * dur.duration / fromIntegral dur.steps
      level = interpolateStep dur 1.0 0.0
  playing <- lift $ asks @PlayerState (.playing)
  stopped <- lift $ atomically (TMap.lookup sn playing)
  case stopped of
    Nothing -> pure mempty
    Just s -> do
      forM_ [0 .. dur.steps] \i -> do
        b <- lift $ update s False (level i) (level i) 0.0 1.0
        unless b $ do
          lift $ atomically (TMap.focus Focus.delete sn playing)
          S.yield $ Right $ Finished $ NE.singleton sn
        unless (i == dur.steps) $ lift $ threadDelay usec
      lift $ atomically (TMap.focus Focus.delete sn playing)
      b <- lift $ stop s
      when b $
        S.yield $
          Right $
            Interrupted $
              NE.singleton sn
      S.yield $ Left Ok

crossFade ::
  ( Reader PlayerEnv :> es
  , Reader PlayerState :> es
  , Audio :> es
  , Concurrent :> es
  ) =>
  Fading ->
  NonEmpty SoundName ->
  NonEmpty SoundName ->
  S.Stream (Of (Either Response Event)) (Eff es) ()
crossFade dur froms tos = do
  smpls <- lift $ asks @PlayerEnv (.samples)
  let usec = floor $ 1_000_000 * dur.duration / fromIntegral dur.steps
      inLevel = interpolateStep dur 0.0 1.0
      outLevel = interpolateStep dur 1.0 0.0
  playing <- lift $ asks @PlayerState (.playing)
  froms' <-
    lift $
      atomically $
        catMaybes
          <$> mapM
            (liftA2 (liftA2 (,)) <$> (pure . pure) <*> flip TMap.lookup playing)
            (NE.toList froms)
  tos' <-
    mapM (mapM (\s -> lift $ play s 0.0 0.0 0.0 1.0)) $
      mapMaybe (liftA2 (,) <$> pure <*> (`OMap.lookup` smpls)) (NE.toList tos)
  S.each $ Right . Started . fmap fst <$> NE.nonEmpty tos'
  forM_ [0 .. dur.steps] \i -> do
    lift $ threadDelay usec
    forM_
      froms'
      ( \(sn, s) -> do
          b <- lift $ update s False (outLevel i) (outLevel i) 0.0 1.0
          unless b do
            lift $ atomically $ TMap.focus Focus.delete sn playing
            S.yield $ Right $ Finished $ NE.singleton sn
      )
    forM_ tos' \(sn, s) -> do
      b <-
        lift $
          update s False (inLevel i) (inLevel i) 0.0 1.0
      unless b $
        lift $
          atomically $
            TMap.insert s sn playing
  mapM_ (lift . stop . snd) froms'
  S.each $ Right . Interrupted . fmap fst <$> NE.nonEmpty froms'
  lift $ do
    forM_ tos' \(sn, s) -> do
      atomically $ TMap.insert s sn playing
    forM_ froms' \(sn, _) -> do
      atomically $ TMap.delete sn playing
  S.yield $ Left Ok

cutIn ::
  ( Reader PlayerEnv :> es
  , Reader PlayerState :> es
  , Audio :> es
  , Concurrent :> es
  ) =>
  SoundName ->
  S.Stream (Of (Either Response Event)) (Eff es) ()
cutIn sn = withSample sn \sample -> do
  playing <- lift $ asks @PlayerState (.playing)
  lift $
    mapM_ soundStop
      =<< atomically (TMap.focus Focus.lookupAndDelete sn playing)
  s <- lift $ play sample 1.0 1.0 0.0 1.0
  S.yield $ Right $ Started $ NE.singleton sn
  lift $ atomically $ TMap.insert s sn playing
  S.yield $ Left Ok

cutOut ::
  ( Reader PlayerState :> es
  , Audio :> es
  , Concurrent :> es
  ) =>
  SoundName ->
  S.Stream (Of (Either Response Event)) (Eff es) ()
cutOut sn = do
  stopped <- lift do
    playing <- asks @PlayerState (.playing)
    stopped <-
      fmap or . mapM stop =<< atomically (TMap.lookup sn playing)
    when stopped $
      atomically $
        TMap.delete sn playing
    pure stopped
  when stopped $ S.yield $ Right $ Interrupted $ NE.singleton sn
  S.yield $ Left Ok

withSample ::
  (Reader PlayerEnv :> es) =>
  SoundName ->
  ( Sample ->
    S.Stream
      (Of (Either Response Event))
      (Eff es)
      ()
  ) ->
  S.Stream (Of (Either Response Event)) (Eff es) ()
withSample sn k = do
  smpls <- lift $ asks @PlayerEnv (.samples)
  maybe (S.yield $ Left $ UnknownSound sn) k $
    OMap.lookup sn smpls
