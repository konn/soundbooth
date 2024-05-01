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
  PlayerOptions (..),
  CueingQueues (..),
  newCueingQueues,
  CueingClientQueues (),
  Config (..),
  subscribeCue,
  runCueingServer,
  readCueEvent,
  readCueResponse,
  sendCueRequest,
) where

import Control.Concurrent.STM.TBMQueue (TBMQueue, closeTBMQueue, newTBMQueue, readTBMQueue, writeTBMQueue)
import Control.Exception.Safe (bracket)
import Control.Foldl qualified as L
import Control.Lens (traverse1, (^.))
import Control.Monad (forM_, guard, when)
import Control.Monad.Trans
import Control.Zipper hiding ((:>))
import Data.Foldable (fold)
import Data.Function
import Data.Functor (void)
import Data.Functor.Of (Of)
import Data.Generics.Labels ()
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Vector qualified as V
import DeferredFolds.UnfoldlM qualified as UL
import Effectful hiding ((:>>))
import Effectful.Concurrent.Async
import Effectful.Concurrent.STM
import Effectful.Concurrent.SinkSource
import Effectful.Random.Static (Random, evalRandom, getStdGen, uniform)
import Effectful.Reader.Static (Reader, runReader)
import Effectful.Reader.Static.Lens qualified as EffL
import Effectful.State.Static.Shared (State, evalState)
import Effectful.State.Static.Shared.Lens
import Focus qualified
import GHC.Generics
import Soundbooth.Common.Types
import Soundbooth.Server.Player
import Soundbooth.Server.Types
import StmContainers.Map qualified as TMap
import StmContainers.Set qualified as TSet
import Streaming qualified as S
import Streaming.Prelude qualified as S

data CueingQueues = CueingQueues
  { cueReqQ :: TBQueue CueRequest
  , playerReqQ :: TBQueue Request
  , incomingEvtQ :: Source Event
  , outgoingEvtQ :: Sink CueEvent
  , incomingRespQ :: Source Response
  , outgoingRespQ :: Sink Response
  }

data CueingClientQueues = CueingClientQueues
  { cueReqQ :: TBQueue CueRequest
  , evtQ :: Source CueEvent
  , respQ :: Source Response
  }
  deriving (Generic)

data CueingState = CueingState {queues :: PlayerQueues}
  deriving (Generic)

newCueingQueues :: (Concurrent :> es) => Eff es (PlayerQueues, CueingQueues)
newCueingQueues = do
  cueReqQ <- newTBQueueIO 100
  reqQ@playerReqQ <- newTBQueueIO 100
  evtQ <- newSinkIO
  incomingEvtQ <- atomically $ subscribeSink evtQ
  respQ <- newSinkIO
  incomingRespQ <- atomically $ subscribeSink respQ
  outgoingEvtQ <- newSinkIO
  outgoingRespQ <- newSinkIO
  pure (PlayerQueues {..}, CueingQueues {..})

subscribeCue :: (Concurrent :> es) => CueingQueues -> Eff es CueingClientQueues
subscribeCue qs = do
  let cueReqQ = qs.cueReqQ
  evtQ <- atomically $ subscribeSink qs.outgoingEvtQ
  respQ <- atomically $ subscribeSink qs.outgoingRespQ
  pure $ CueingClientQueues {..}

runCueingServer ::
  (IOE :> es, Concurrent :> es) =>
  PlayerOptions ->
  PlayerQueues ->
  CueingQueues ->
  Config ->
  Eff es ()
runCueingServer playerOpts pqs qs cfg = do
  nowPlaying <- liftIO TSet.newIO
  subscription <- liftIO TMap.newIO
  let cueTape = traverse `within` zipper cfg.cues
      trackTape = Nothing
      fadeOut = Nothing
      status = Idle
      cuelist = buildCuelist cfg.cues
  g <- getStdGen
  evalRandom g $
    evalState CueState {..} $
      runReader CueEnv {..} $
        relayRequest qs
          `race_` relayEvent qs
          `race_` relayResponse qs
          `race_` runPlayer playerOpts pqs cfg

buildCuelist :: V.Vector RawCue -> Cuelist
buildCuelist = V.map \Cue {..} ->
  let steps =
        V.map
          \case
            PlayCue {..} -> Plays {sounds = play, ..}
            CrossFadeTo {..} -> CrossFades {sounds = crossFadeTo, ..}
          $ V.fromList
          $ NE.toList commands
   in CueInfo {..}

relayRequest :: (Concurrent :> es, State CueState :> es, Reader CueEnv :> es, Random :> es) => CueingQueues -> Eff es ()
relayRequest qs =
  S.repeatM (atomically $ readTBQueue qs.cueReqQ)
    & flip S.for process
    & S.mapM_ (atomically . either (sendCueEvent qs) (sendPlayerRequest qs))

relayResponse :: (Concurrent :> es) => CueingQueues -> Eff es ()
relayResponse qs =
  S.repeatM (atomically $ readSource qs.incomingRespQ)
    & S.mapM_ (atomically . writeSink qs.outgoingRespQ)

sendCueEvent :: CueingQueues -> CueEvent -> STM ()
sendCueEvent CueingQueues {..} = writeSink outgoingEvtQ

sendPlayerRequest :: CueingQueues -> Request -> STM ()
sendPlayerRequest CueingQueues {..} = writeTBQueue playerReqQ

process ::
  ( State CueState :> es
  , Reader CueEnv :> es
  , Concurrent :> es
  , Random :> es
  ) =>
  CueRequest ->
  S.Stream (S.Of (Either CueEvent Request)) (Eff es) ()
process (PlayerRequest req) = S.yield $ Right req
process CuePlay = startCue
process CueStop = stopCue True
process CueGoNext = stopCue True
process CueGoPrev = do
  stopCue False
  lift $ #cueTape %= fmap (tug leftward)
process (CueGoto i) = do
  stopCue False
  lift $ #cueTape %= fmap (tug (moveTo i))
process GetCuelist = do
  S.yield . Left . CueCurrentCues =<< lift (EffL.view #cuelist)

data CueEnv = CueEnv
  { nowPlaying :: TSet.Set SoundName
  , subscription :: TMap.Map SoundName (TMap.Map Int (TBMQueue Event))
  , cuelist :: Cuelist
  }
  deriving (Generic)

data CueState = CueState
  { cueTape :: Maybe (Top :>> V.Vector RawCue :>> RawCue)
  , trackTape :: Maybe (Top :>> V.Vector RawCue :>> RawCue :>> NonEmpty RawCueCommand :>> RawCueCommand)
  , fadeOut :: Maybe Fading
  , status :: Status
  }
  deriving (Generic)

relayEvent ::
  (Concurrent :> es, Reader CueEnv :> es) =>
  CueingQueues ->
  Eff es ()
relayEvent CueingQueues {..} =
  S.repeatM (atomically $ readSource incomingEvtQ)
    & S.chain
      ( \evt -> do
          subs <- EffL.view #subscription
          playing <- EffL.view #nowPlaying
          atomically do
            forM_ (eventSounds evt) \sn -> do
              case evt of
                Finished {} -> TSet.delete sn playing
                Interrupted {} -> TSet.delete sn playing
                Started {} -> TSet.insert sn playing
                CurrentPlaylist {} -> pure ()
                KeepAlive -> pure ()
              mtargs <- TMap.lookup sn subs
              forM_ mtargs \targs ->
                UL.foldM (L.mapM_ $ (`writeTBMQueue` evt) . snd) (TMap.unfoldlM targs)
      )
    & S.mapM_ (atomically . writeSink outgoingEvtQ . PlayerEvent)

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
                  pure $ dic <$ guard (not isNull)
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

stopCue ::
  (State CueState :> es, Reader CueEnv :> es, Concurrent :> es) =>
  -- | Goto next cue after stopping?
  Bool ->
  S.Stream (Of (Either CueEvent Request)) (Eff es) ()
stopCue goNext = do
  mCue <- lift $ use #cueTape
  forM_ mCue \aCue -> do
    st <- lift $ use #status
    case st of
      Idle -> pure ()
      Playing -> do
        lift $ #status .= Idle
        lift $ #trackTape .= Nothing
        when goNext $
          lift $
            #cueTape ?= tug rightward aCue
        fading <- lift $ use #fadeOut <* (#fadeOut .= Nothing)
        nowPl <- lift $ EffL.view #nowPlaying
        playing <-
          lift $
            atomically $
              UL.foldM (L.generalize L.set) (TSet.unfoldlM nowPl)
                <* TSet.reset nowPl
        mapM_ (S.yield . Right . maybe Stop FadeOut fading) playing
        pure undefined

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
                  ( fmap (fmap Right) . maybe
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
                  UL.foldM (NE.nonEmpty <$> L.generalize L.nub) (TSet.unfoldlM playing)
                    <* TSet.reset playing
              case froms0 of
                Nothing ->
                  S.each $
                    Right . FadeIn crossFade <$> crossFadeTo
                Just froms ->
                  S.yield $
                    Right $
                      CrossFade crossFade froms crossFadeTo
              lift $ #fadeOut .= fadeOut
              pure crossFadeTo
          st' <-
            lift $
              fold
                <$> mapConcurrently
                  ( \name ->
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

readCueEvent :: CueingClientQueues -> STM CueEvent
readCueEvent CueingClientQueues {..} = readSource evtQ

readCueResponse :: CueingClientQueues -> STM Response
readCueResponse CueingClientQueues {..} = readSource respQ

sendCueRequest :: CueingClientQueues -> CueRequest -> STM ()
sendCueRequest CueingClientQueues {..} = writeTBQueue cueReqQ
