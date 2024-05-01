{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Soundbooth.Common.Types (
  SoundName (..),
  CueRequest (..),
  Request (..),
  Response (..),
  Event (..),
  eventSounds,
  Status (..),
  Interpolation (..),
  Playlist (..),
  Fading (..),
  CueEvent (..),
  CueInfo (..),
  CueStep (..),
  CueID,
  CueStatus (..),
  CueingStatus (..),
  Cuelist,
) where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
import Data.Aeson
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.String (IsString)
import Data.Text (Text)
import Data.Vector qualified as V
import GHC.Generics (Generic)

newtype SoundName = SoundName Text
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, ToJSON, IsString, Hashable, NFData)

newtype Playlist = Playlist
  { sounds :: V.Vector (SoundName, Status)
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

data Status = Idle | Playing
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData, Hashable)

data Fading = Fading {duration :: !Double, steps :: !Int, interpolation :: !(Maybe Interpolation)}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

data Interpolation = Linear | Quadratic
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

data Request
  = Play !SoundName
  | Stop !SoundName
  | FadeIn !Fading !SoundName
  | FadeOut !Fading !SoundName
  | CrossFade !Fading !(NonEmpty SoundName) !(NonEmpty SoundName)
  | StopAll
  | GetPlaylist
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Response = Ok | UnknownSound !SoundName | Error !Text
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data CueInfo = CueInfo {name :: !Text, steps :: V.Vector CueStep}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data CueStep
  = Plays
      { sounds :: !(NonEmpty SoundName)
      , fadeIn :: !(Maybe Fading)
      , fadeOut :: !(Maybe Fading)
      }
  | CrossFades
      { sounds :: !(NonEmpty SoundName)
      , crossFade :: !Fading
      , fadeOut :: !(Maybe Fading)
      }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

type Cuelist = V.Vector CueInfo

data CueRequest
  = CuePlay
  | CueStop
  | CueGoPrev
  | CueGoNext
  | CueGoto !Int
  | GetCueState
  | PlayerRequest !Request
  deriving (Show, Eq, Ord, Generic)

instance FromJSON CueRequest where
  parseJSON v =
    flip (withText "CuePlay") v \case
      "CuePlay" -> pure CuePlay
      "CueStop" -> pure CueStop
      "CueGoPrev" -> pure CueGoPrev
      "CueGoNext" -> pure CueGoNext
      _ -> fail "invalid CueRequest"
      <|> flip
        (withObject "CueRequest")
        v
        \o -> CueGoto <$> o .: "CueGoto" <|> PlayerRequest <$> parseJSON (Object o)

instance ToJSON CueRequest where
  toJSON CuePlay = "CuePlay"
  toJSON CueStop = "CueStop"
  toJSON CueGoNext = "CueGoNext"
  toJSON CueGoPrev = "CueGoPrev"
  toJSON GetCueState = "GetCuelist"
  toJSON (CueGoto i) = object ["CueGoto" .= i]
  toJSON (PlayerRequest req) = toJSON req

data Event
  = Started !(NonEmpty SoundName)
  | Interrupted !(NonEmpty SoundName)
  | Finished !(NonEmpty SoundName)
  | CurrentPlaylist !Playlist
  | KeepAlive
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

eventSounds :: Event -> [SoundName]
eventSounds (Started sns) = NE.toList sns
eventSounds (Interrupted sns) = NE.toList sns
eventSounds (Finished sns) = NE.toList sns
eventSounds CurrentPlaylist {} = []
eventSounds KeepAlive = []

type CueID = Int

data CueStatus = IdleCue | CuePlayingStep !Int | CueFinished | CueInterrupted !Int
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data CueingStatus = Inactive | Active CueID CueInfo CueStatus
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data CueEvent
  = CueStatus !CueingStatus
  | CueCurrentCues !Cuelist
  | PlayerEvent !Event
  deriving (Show, Eq, Ord, Generic)

instance ToJSON CueEvent where
  toJSON (PlayerEvent e) = toJSON e
  toJSON (CueStatus i) = object ["CueStatus" .= i]
  toJSON (CueCurrentCues i) = object ["CueCurrentCues" .= i]

instance FromJSON CueEvent where
  parseJSON = withObject "CueEvent" $ \o -> do
    CueStatus <$> o .: "CueStatus"
      <|> CueCurrentCues <$> o .: "CueCurrentCues"
      <|> PlayerEvent <$> parseJSON (Object o)
