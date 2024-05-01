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
  CueRequest (..),
  Request (..),
  Response (..),
  Event (..),
  Status (..),
  Interpolation (..),
  Playlist (..),
  Fading (..),
  CueEvent (..),
) where

import Control.Applicative ((<|>))
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
  | GetCues
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Response = Ok | UnknownSound !SoundName | Error !Text
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

type Cuelist = V.Vector (SoundName, Status)

data CueRequest
  = CuePlay !Int
  | CueStop !Int
  | CueGoto !Int
  | PlayerRequest !Request
  deriving (Show, Eq, Ord, Generic)

instance FromJSON CueRequest where
  parseJSON = withObject "CueRequest" $ \o -> do
    CuePlay <$> o .: "CuePlay"
      <|> CueStop <$> o .: "CueStop"
      <|> CueGoto <$> o .: "CueGoto"
      <|> PlayerRequest <$> parseJSON (Object o)

instance ToJSON CueRequest where
  toJSON (CuePlay i) = object ["CuePlay" .= i]
  toJSON (CueStop i) = object ["CueStop" .= i]
  toJSON (CueGoto i) = object ["CueGoto" .= i]
  toJSON (PlayerRequest req) = toJSON req

data Event
  = Started !(NonEmpty SoundName)
  | Interrupted !(NonEmpty SoundName)
  | Finished !(NonEmpty SoundName)
  | CurrentPlaylist !Playlist
  | CurrentCues !Cuelist
  | KeepAlive
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data CueEvent
  = CueStatus !Int
  | PlayerEvent !Event
  deriving (Show, Eq, Ord, Generic)

instance ToJSON CueEvent where
  toJSON (PlayerEvent e) = toJSON e
  toJSON (CueStatus i) = object ["CueStatus" .= i]

instance FromJSON CueEvent where
  parseJSON = withObject "CueEvent" $ \o -> do
    CueStatus <$> o .: "CueStatus"
      <|> PlayerEvent <$> parseJSON (Object o)
