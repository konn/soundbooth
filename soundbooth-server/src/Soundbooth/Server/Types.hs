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

module Soundbooth.Server.Types (
  Config (..),
  Music (..),
  Cue' (..),
  RawCue (),
  Cue (),
  CueCommand' (..),
  CueCommand,
  NamedSample (..),
  RawCueCommand,
  commandTargets,
  SoundName (..),
  Request (..),
  CueRequest (..),
  Response (..),
  Event (..),
  newPlayerQueues,
  PlayerQueues (..),
  subscribe,
  ClientQueues (),
  readEvent,
  readResponse,
  sendRequest,
  sendEvent,
) where

import Data.Aeson qualified as J
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Vector qualified as V
import Effectful
import Effectful.Audio
import Effectful.Concurrent.Async
import Effectful.Concurrent.STM
import Effectful.Concurrent.SinkSource
import GHC.Generics
import Numeric.Natural
import Soundbooth.Common.Types

data Config = Config {files :: V.Vector Music, cues :: V.Vector RawCue}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON)

data Music = Music {name :: !SoundName, path :: !Text}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON)

type RawCue = Cue' SoundName

type Cue = Cue' NamedSample

data CueState
  = NoCue
  | Paused !Int
  | Cued !Int
  deriving (Show, Eq, Ord, Generic)

data Cue' a = Cue
  { name :: Text
  , commands :: NonEmpty (CueCommand' a)
  }
  deriving (Show, Eq, Ord, Generic, Generic1, Functor, Foldable, Traversable)

deriving anyclass instance J.ToJSON RawCue

instance J.FromJSON RawCue where
  parseJSON = J.withObject "{cue}" \o -> do
    name <- o J..: "name"
    commands <- o J..: "commands"
    pure Cue {..}

type RawCueCommand = CueCommand' SoundName

data NamedSample = NamedSample {name :: !SoundName, sample :: !Sample}
  deriving (Generic)

type CueCommand = CueCommand' NamedSample

data CueCommand' a
  = PlayCue
      { play :: NonEmpty a
      , fadeIn :: Maybe Fading
      , fadeOut :: Maybe Fading
      }
  | CrossFadeTo
      { crossFadeTo :: NonEmpty a
      , crossFade :: !Fading
      , fadeOut :: Maybe Fading
      }
  deriving (Show, Eq, Ord, Generic, Generic1, Functor, Foldable, Traversable)

commandTargets :: CueCommand' a -> NonEmpty a
commandTargets = \case
  PlayCue {play = songs} -> songs
  CrossFadeTo {crossFadeTo} -> crossFadeTo

cmdOpts :: J.Options
cmdOpts = J.defaultOptions {J.sumEncoding = J.UntaggedValue}

instance J.FromJSON RawCueCommand where
  parseJSON = J.genericParseJSON cmdOpts

instance J.ToJSON RawCueCommand where
  toJSON = J.genericToJSON cmdOpts

data PlayerQueues = PlayerQueues
  { evtQ :: Sink Event
  , respQ :: Sink Response
  , reqQ :: TBQueue Request
  }

data ClientQueues = ClientQueues
  { evtQ :: Source Event
  , respQ :: Source Response
  , reqQ :: TBQueue Request
  }

sendEvent :: PlayerQueues -> Event -> STM ()
sendEvent PlayerQueues {evtQ} = writeSink evtQ

readEvent :: ClientQueues -> STM Event
readEvent ClientQueues {evtQ} = readSource evtQ

readResponse :: ClientQueues -> STM Response
readResponse ClientQueues {respQ} = readSource respQ

sendRequest :: ClientQueues -> Request -> STM ()
sendRequest ClientQueues {reqQ} = writeTBQueue reqQ

subscribe :: (Concurrent :> es) => PlayerQueues -> Eff es ClientQueues
subscribe qs = do
  evtQ <- atomically $ subscribeSink qs.evtQ
  respQ <- atomically $ subscribeSink qs.respQ
  pure ClientQueues {reqQ = qs.reqQ, ..}

newPlayerQueues :: (Concurrent :> es) => Natural -> Eff es PlayerQueues
newPlayerQueues size = do
  evtQ <- newSinkIO
  respQ <- newSinkIO
  reqQ <- newTBQueueIO size
  pure PlayerQueues {..}
