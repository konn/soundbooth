{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Soundbooth.Common.Types (
  SoundName (..),
  Request (..),
  Response (..),
  Event (..),
  Status (..),
  Playlist (..),
  Fading (..),
) where

import Control.DeepSeq (NFData)
import Data.Aeson
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty)
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

data Fading = Fading {duration :: !Double, steps :: !Int}
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
  | GetCues
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Response = Ok | UnknownSound !SoundName | Error !Text
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

type Cuelist = V.Vector (SoundName, Status)

data Event
  = Started !(NonEmpty SoundName)
  | Stopped !(NonEmpty SoundName)
  | CurrentPlaylist !Playlist
  | CurrentCues !Cuelist
  | KeepAlive
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)
