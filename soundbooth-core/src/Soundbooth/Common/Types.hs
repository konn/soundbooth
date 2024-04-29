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
  = Started !(NonEmpty SoundName)
  | Stopped !(NonEmpty SoundName)
  | CurrentPlaylist !Playlist
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)
