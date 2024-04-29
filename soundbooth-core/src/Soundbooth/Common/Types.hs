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
) where

import Control.DeepSeq (NFData)
import Data.Aeson
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty)
import Data.String (IsString)
import Data.Text (Text)
import GHC.Generics (Generic)

newtype SoundName = SoundName Text
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, ToJSON, IsString, Hashable, NFData)

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
