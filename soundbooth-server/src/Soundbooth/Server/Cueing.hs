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
{-# OPTIONS_GHC -fplugin Effectful.Plugin #-}

module Soundbooth.Server.Cueing (
  CueingQueues (..),
  newCueingQueues,
  CueingClientQueues (),
  subscribeCue,
  toPlayerQueues,
  runCueingServer,
) where

import Control.Arrow ((>>>))
import Control.Concurrent.STM.TBMQueue (TBMQueue, closeTBMQueue, newTBMQueue, readTBMQueue)
import Control.Exception.Safe (bracket)
import Control.Foldl qualified as L
import Control.Lens (preview, traverse1, view, (^.))
import Control.Monad (forM)
import Control.Monad.Loops (whileJust_)
import Control.Monad.Trans
import Control.Zipper hiding ((:>))
import Data.ByteString qualified as BS
import Data.Foldable (fold)
import Data.Foldable qualified as F
import Data.Function
import Data.Functor (void)
import Data.Functor.Of (Of)
import Data.Generics.Labels ()
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Vector qualified as V
import DeferredFolds.UnfoldlM qualified as UL
import Effectful hiding ((:>>))
import Effectful.Audio (Sample)
import Effectful.Concurrent.Async
import Effectful.Concurrent.STM
import Effectful.Random.Static (Random, uniform)
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static.Lens qualified as EffL
import Effectful.State.Static.Shared (State)
import Effectful.State.Static.Shared.Lens
import Focus qualified
import GHC.Generics
import Soundbooth.Common.Types
import Soundbooth.Server.Player
import Soundbooth.Server.Types
import StmContainers.Map qualified as TMap
import Streaming qualified as S
import Streaming.Prelude qualified as S

data CueingQueues = CueingQueues
  { cueReqQ :: TBQueue CueRequest
  , playerReqQ :: TBQueue Request
  , evtQ :: TChan Event
  , respQ :: TChan Response
  }

data CueingClientQueues = CueingClientQueues
  { cueReqQ :: TBQueue CueRequest
  , evtQ :: TChan Event
  , respQ :: TChan Response
  }
  deriving (Generic)

data CueingState = CueingState {queues :: PlayerQueues}
  deriving (Generic)

newCueingQueues :: (Concurrent :> es) => Eff es CueingQueues
newCueingQueues = do
  cueReqQ <- newTBQueueIO 100
  playerReqQ <- newTBQueueIO 100
  evtQ <- newBroadcastTChanIO
  respQ <- newBroadcastTChanIO
  pure CueingQueues {..}

toPlayerQueues :: CueingQueues -> PlayerQueues
toPlayerQueues qs =
  let evtQ = qs.evtQ
      respQ = qs.respQ
      reqQ = qs.playerReqQ
   in PlayerQueues {..}

subscribeCue :: (Concurrent :> es) => CueingQueues -> Eff es CueingClientQueues
subscribeCue qs = do
  let cueReqQ = qs.cueReqQ
  evtQ <- atomically $ dupTChan qs.evtQ
  respQ <- atomically $ dupTChan qs.respQ
  pure $ CueingClientQueues {..}

runCueingServer ::
  (IOE :> es, Concurrent :> es) =>
  PlayerOptions ->
  CueingQueues ->
  Config ->
  Eff es ()
runCueingServer playerOpts qs cs = do
  runPlayer playerOpts (toPlayerQueues qs) cs
    `race_` relay qs

relay :: (Concurrent :> es) => CueingQueues -> Eff es ()
relay qs =
  S.repeatM (atomically $ readTBQueue qs.cueReqQ)
    & flip S.for process
    & S.mapM_ (atomically . writeTBQueue qs.playerReqQ)

process :: CueRequest -> S.Stream (S.Of Request) (Eff es) ()
process (PlayerRequest req) = S.yield req
process _ = mempty

data CueEnv = CueEnv
  { nowPlaying :: TMap.Map SoundName Sample
  , subscription :: TMap.Map SoundName (TMap.Map Int (TBMQueue Event))
  }
  deriving (Generic)

data CueState = CueState
  { cueTape :: Maybe (Top :>> V.Vector Cue :>> Cue)
  , trackTape :: Maybe (Top :>> V.Vector Cue :>> Cue :>> NonEmpty CueCommand :>> CueCommand)
  , fadeOut :: Maybe Fading
  , status :: Status
  }
  deriving (Generic)

subscribeSound ::
  (Concurrent :> es, Reader CueEnv :> es, Random :> es) =>
  SoundName ->
  (TBMQueue Event -> Eff es a) ->
  Eff es a
subscribeSound name =
  bracket
    ( do
        subs <- EffL.view #subscription
        uuid <- uniform
        atomically $ do
          q <- newTBMQueue 100
          TMap.focus
            ( Focus.alterM $
                fmap Just
                  . maybe
                    (do dic <- TMap.new; dic <$ TMap.insert q uuid dic)
                    (\dic -> dic <$ TMap.insert q uuid dic)
            )
            name
            subs
          pure (q, uuid)
    )
    ( \(q, uuid) -> do
        subs <- EffL.view #subscription
        atomically $ do
          closeTBMQueue q
          TMap.focus
            ( Focus.alterM \case
                Nothing -> pure Nothing
                Just dic -> do
                  TMap.delete uuid dic
                  isNull <- TMap.null dic
                  if isNull
                    then pure Nothing
                    else pure $ Just dic
            )
            name
            subs
    )
    . (. fst)

data St = Done | Abort
  deriving (Show, Eq, Ord, Generic)

instance Semigroup St where
  Abort <> _ = Abort
  _ <> Abort = Abort
  Done <> r = r

instance Monoid St where
  mempty = Done

startCue ::
  (State CueState :> es, Reader CueEnv :> es, Concurrent :> es, Random :> es) =>
  S.Stream (Of (Either CueEvent Request)) (Eff es) ()
startCue = do
  aCue <- lift $ use #cueTape
  lift $ #trackTape .= (within traverse1 . downward #commands =<< aCue)
  void $
    Nothing & fix \self currentSt -> do
      mcursor <- lift $ use #trackTape
      case mcursor of
        Nothing -> pure currentSt
        Just cursor -> do
          targets <- case cursor ^. focus of
            PlayCue {..} -> do
              S.each $
                fmap
                  ( (.name)
                      >>> fmap (fmap Right) . maybe
                        <$> Play
                        <*> flip FadeIn
                        <*> pure fadeIn
                  )
                  play
              lift $ #fadeOut .= fadeOut
              pure play
            CrossFadeTo {..} -> do
              froms0 <- lift $ do
                playing <- EffL.view #nowPlaying
                atomically $
                  UL.foldM (L.generalize $ L.premap fst L.set) (TMap.unfoldlM playing)
                    <* TMap.reset playing
              case NE.nonEmpty $ F.toList froms0 of
                Nothing ->
                  S.each $
                    Right . FadeIn crossFade . (.name) <$> crossFadeTo
                Just froms ->
                  S.yield $
                    Right $
                      CrossFade crossFade froms $
                        (.name) <$> crossFadeTo
              lift $ #fadeOut .= fadeOut
              pure crossFadeTo
          st' <-
            lift $
              fold
                <$> mapConcurrently
                  ( \NamedSample {..} ->
                      subscribeSound name \evts -> fix \go -> do
                        r <- atomically $ readTBMQueue evts
                        maybe
                          (pure Done)
                          ( \case
                              Finished {} -> pure Done
                              Interrupted {} -> pure Abort
                              _ -> go
                          )
                          r
                  )
                  targets
          case st' of
            Abort -> pure (Just Abort)
            _ -> do
              lift $ #trackTape .= rightward cursor
              self $ Just st'
  lift $ #cueTape .= (rightward =<< aCue)
  lift $ #trackTape .= Nothing
  lift $ #status .= Idle
