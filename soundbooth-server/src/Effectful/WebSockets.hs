{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- A port of proteaaudio to effectful
module Effectful.WebSockets (
  WebSockets,
  runWebSocketsIO,
  Connection,
  WebSocketsData (..),
  sendTextData,
  sendBinaryData,
  sendPing,
  receiveData,
  sendClose,
) where

import Effectful
import Effectful.Dispatch.Dynamic
import Network.WebSockets (Connection, WebSocketsData (..))
import Network.WebSockets qualified as WS

data WebSockets :: Effect where
  SendTextData :: (WebSocketsData a) => Connection -> a -> WebSockets r ()
  SendBinaryData :: (WebSocketsData a) => Connection -> a -> WebSockets r ()
  SendPing :: (WebSocketsData a) => Connection -> a -> WebSockets r ()
  ReceiveData :: (WebSocketsData a) => Connection -> WebSockets r a
  SendClose :: (WebSocketsData a) => Connection -> a -> WebSockets r ()

type instance DispatchOf WebSockets = 'Dynamic

sendTextData :: (WebSocketsData a, WebSockets :> es) => Connection -> a -> Eff es ()
sendTextData = fmap send . SendTextData

sendBinaryData :: (WebSocketsData a, WebSockets :> es) => Connection -> a -> Eff es ()
sendBinaryData = fmap send . SendBinaryData

receiveData :: (WebSocketsData a, WebSockets :> es) => Connection -> Eff es a
receiveData = send . ReceiveData

sendPing :: (WebSocketsData a, WebSockets :> es) => Connection -> a -> Eff es ()
sendPing = fmap send . SendPing

sendClose :: (WebSocketsData a, WebSockets :> es) => Connection -> a -> Eff es ()
sendClose = fmap send . SendClose

runWebSocketsIO :: (IOE :> es) => Eff (WebSockets : es) a -> Eff es a
runWebSocketsIO = interpret $ \_ -> \case
  SendTextData conn a -> liftIO $ WS.sendTextData conn a
  SendBinaryData conn a -> liftIO $ WS.sendBinaryData conn a
  ReceiveData conn -> liftIO $ WS.receiveData conn
  SendPing conn p -> liftIO $ WS.sendPing conn p
  SendClose conn p -> liftIO $ WS.sendClose conn p
