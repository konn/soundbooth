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
import Control.Lens (traverse1, view, (^.))
import Control.Lens.Extras (is)
import Control.Monad (forM_, guard, when)
import Control.Monad.Trans
import Control.Zipper hiding ((:>))
import Data.Foldable (fold)
import Data.Function
import Data.Functor (void)
import Data.Generics.Labels ()
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Vector qualified as V
import DeferredFolds.UnfoldlM qualified as UL
import Effectful hiding ((:>>))
import Effectful.Concurrent.Async
import Effectful.Concurrent.STM
import Effectful.Concurrent.SinkSource
import Effectful.Log.Extra (Log, localDomain, logTrace, logTrace_)
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
import Streaming.Prelude qualified as S

data CueingQueues = CueingQueues
  { cueReqQ :: TBQueue CueRequest
  , playerReqQ :: TBQueue Request
  , incomingEvtQ :: Source Event
  , outgoingEvtQ :: Sink CueEvent
  , incomingRespQ :: Source Response
  , outgoingRespQ :: Sink Response
  }
  deriving (Generic)

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
  (IOE :> es, Concurrent :> es, Log :> es) =>
  PlayerOptions ->
  PlayerQueues ->
  CueingQueues ->
  Config ->
  Eff es ()
runCueingServer playerOpts pqs qs@cueingQueues cfg = do
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
buildCuelist = V.map toCueInfo

toCueInfo :: Cue' SoundName -> CueInfo
toCueInfo Cue {..} =
  let steps =
        V.map
          \case
            PlayCue {..} -> Plays {sounds = play, ..}
            CrossFadeTo {..} -> CrossFades {sounds = crossFadeTo, ..}
          $ V.fromList
          $ NE.toList commands
   in CueInfo {..}

relayRequest :: (Concurrent :> es, State CueState :> es, Reader CueEnv :> es, Random :> es, Log :> es) => CueingQueues -> Eff es ()
relayRequest qs =
  S.repeatM (atomically $ readTBQueue qs.cueReqQ)
    & S.mapM_ process

relayResponse :: (Concurrent :> es) => CueingQueues -> Eff es ()
relayResponse qs =
  S.repeatM (atomically $ readSource qs.incomingRespQ)
    & S.mapM_ (atomically . writeSink qs.outgoingRespQ)

process ::
  ( State CueState :> es
  , Reader CueEnv :> es
  , Concurrent :> es
  , Random :> es
  , Log :> es
  ) =>
  CueRequest ->
  Eff es ()
process (PlayerRequest req) = localDomain "process" $ do
  logTrace "received player request" req
  when (is #_StopAll req) $ stopCue True
  pushPlayerRequest req
process CuePlay = do
  localDomain "process" $
    logTrace_ "Cue Playing"
  void $ async startCue
process CueStop = localDomain "process" $ do
  logTrace_ "Cue Stopping"
  stopCue True
process CueGoNext = localDomain "process" do
  logTrace_ "Cue Going next"
  switchCuePossiblyCrossFade $ fmap (tug rightward)
process CueGoPrev = localDomain "process" do
  logTrace_ "Cue Going Prev"
  switchCuePossiblyCrossFade $ fmap (tug leftward)
process (CueGoto i) = localDomain "process" do
  logTrace "Cue GoTo" i
  switchCuePossiblyCrossFade $ fmap (tug (moveTo i))
process GetCueState = localDomain "process" do
  logTrace_ "GetCueState"
  sendCueEvent . CueCurrentCues =<< EffL.view #cuelist
  sendCueEvent . CueStatus =<< getCueStatus

switchCuePossiblyCrossFade ::
  ( State CueState :> es
  , Reader CueEnv :> es
  , Concurrent :> es
  , Random :> es
  , Log :> es
  ) =>
  (Maybe CueTape -> Maybe CueTape) ->
  Eff es ()
switchCuePossiblyCrossFade f = do
  stt <- use #status
  case stt of
    Idle -> #cueTape %= f
    Playing -> do
      #cueTape %= f
      void $ async startCue

getCueStatus ::
  (State CueState :> es) =>
  Eff es CueingStatus
getCueStatus = do
  mcue <- use #cueTape
  case mcue of
    Nothing -> pure Inactive
    Just cue -> do
      mtrack <- use #trackTape
      let pos = tooth cue
          cueInfo = toCueInfo $ cue ^. focus
      case mtrack of
        Nothing -> pure $ Active pos cueInfo IdleCue
        Just track -> do
          status <- use #status
          pure $
            Active pos cueInfo $
              case status of
                Idle -> IdleCue
                Playing -> CuePlayingStep $ tooth track

data CueEnv = CueEnv
  { nowPlaying :: TSet.Set SoundName
  , subscription :: TMap.Map SoundName (TMap.Map Int (TBMQueue Event))
  , cuelist :: Cuelist
  , cueingQueues :: CueingQueues
  }
  deriving (Generic)

type CueTape = Top :>> V.Vector RawCue :>> RawCue

type TrackTape = Top :>> V.Vector RawCue :>> RawCue :>> NonEmpty RawCueCommand :>> RawCueCommand

data CueState = CueState
  { cueTape :: Maybe (CueTape)
  , trackTape :: Maybe TrackTape
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
  (State CueState :> es, Reader CueEnv :> es, Concurrent :> es, Log :> es) =>
  -- | Goto next cue after stopping?
  Bool ->
  Eff es ()
stopCue goNext = localDomain "stopCue" $ do
  mCue <- use #cueTape
  st <- use #status
  logTrace "stopping cue" (view focus <$> mCue, st)
  case st of
    Idle -> pure ()
    Playing -> do
      #status .= Idle
      aTrack <- use #trackTape
      #trackTape .= Nothing
      atomically . TSet.reset =<< EffL.view #nowPlaying
      forM_ (commandTargets . view focus <$> aTrack) \sds -> do
        fading <- use #fadeOut <* (#fadeOut .= Nothing)
        forConcurrently_ sds $ pushPlayerRequest . maybe Stop FadeOut fading
      forM_ mCue \aCue ->
        when goNext $ #cueTape ?= tug rightward aCue

sendCueEvent :: (Reader CueEnv :> es, Concurrent :> es) => CueEvent -> Eff es ()
sendCueEvent ce = do
  outEvtQ <- EffL.view $ #cueingQueues . #outgoingEvtQ
  atomically $ writeSink outEvtQ ce

pushPlayerRequest :: (Reader CueEnv :> es, Concurrent :> es) => Request -> Eff es ()
pushPlayerRequest ce = do
  plEvtQ <- EffL.view $ #cueingQueues . #playerReqQ
  atomically $ writeTBQueue plEvtQ ce

startCue ::
  (State CueState :> es, Reader CueEnv :> es, Concurrent :> es, Random :> es, Log :> es) =>
  Eff es ()
startCue = localDomain "startCue" do
  st <- use #status
  when (st == Playing) $ void $ stopCue False
  aCue <- use #cueTape
  logTrace_ $ "playing: " <> tshow (view focus <$> aCue)
  #trackTape .= (within traverse1 . downward #commands =<< aCue)
  #status .= Playing
  void $
    Nothing & fix \self currentSt -> do
      sendCueEvent . CueStatus =<< getCueStatus
      mcursor <- use #trackTape
      case mcursor of
        Nothing -> pure currentSt
        Just cursor -> do
          targets <- case cursor ^. focus of
            PlayCue {..} -> do
              mapConcurrently_
                ( fmap (fmap pushPlayerRequest) . maybe
                    <$> Play
                    <*> flip FadeIn
                    <*> pure fadeIn
                )
                play
              #fadeOut .= fadeOut
              pure play
            CrossFadeTo {..} -> do
              froms0 <- do
                playing <- EffL.view #nowPlaying
                atomically $
                  UL.foldM (NE.nonEmpty <$> L.generalize L.nub) (TSet.unfoldlM playing)
                    <* TSet.reset playing
              case froms0 of
                Nothing ->
                  mapConcurrently_ (pushPlayerRequest . FadeIn crossFade) crossFadeTo
                Just froms ->
                  pushPlayerRequest $ CrossFade crossFade froms crossFadeTo
              #fadeOut .= fadeOut
              pure crossFadeTo
          st' <-
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
              #trackTape .= rightward cursor
              self $ Just st'
  #cueTape .= (rightward =<< aCue)
  #trackTape .= Nothing
  #status .= Idle
  sendCueEvent . CueStatus =<< getCueStatus

tshow :: (Show a) => a -> T.Text
tshow = T.pack . show

readCueEvent :: CueingClientQueues -> STM CueEvent
readCueEvent CueingClientQueues {..} = readSource evtQ

readCueResponse :: CueingClientQueues -> STM Response
readCueResponse CueingClientQueues {..} = readSource respQ

sendCueRequest :: CueingClientQueues -> CueRequest -> STM ()
sendCueRequest CueingClientQueues {..} = writeTBQueue cueReqQ
