{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Soundbooth.Server.Orphans () where

import Data.Aeson
import Network.WebSockets hiding (Request, Response)
import Soundbooth.Common.Types

newtype ViaJSON a = ViaJSON a

instance (ToJSON a, FromJSON a) => WebSocketsData (ViaJSON a) where
  fromDataMessage (Text bs _) = case eitherDecode' bs of
    Left err -> error err
    Right a -> ViaJSON a
  fromDataMessage (Binary bs) = case eitherDecode' bs of
    Left err -> error err
    Right a -> ViaJSON a
  toLazyByteString (ViaJSON a) = encode a
  fromLazyByteString bs = case eitherDecode' bs of
    Left err -> error err
    Right a -> ViaJSON a

deriving via ViaJSON Request instance WebSocketsData Request

deriving via ViaJSON Response instance WebSocketsData Response

deriving via ViaJSON Event instance WebSocketsData Event
