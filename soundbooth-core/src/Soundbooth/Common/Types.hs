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
import Data.Aeson.Micro (FromJSON, ToJSON)
import Data.Aeson.Micro.Generics ()
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty)
import Data.String (IsString)
import Data.Text (Text)
import GHC.Generics (Generic, Generically (..))

newtype SoundName = SoundName Text
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, ToJSON, IsString, Hashable, NFData)

data Request
  = Play !SoundName
  | Stop !SoundName
  | StopAll
  deriving (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via Generically Request

data Response = Ok | UnknownSound !SoundName | Error !Text
  deriving (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via Generically Response

data Event
  = Playing !(NonEmpty SoundName)
  | Stopped !(NonEmpty SoundName)
  deriving (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via Generically Event
