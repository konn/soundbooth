module Effectful.Concurrent.SinkSource (
  Sink,
  Source,
  subscribeSink,
  newSink,
  newSinkIO,
  writeSink,
  readSource,
) where

import Effectful
import Effectful.Concurrent.STM
import GHC.Generics

newtype Sink a = Sink {rawSink :: TChan a}
  deriving (Eq, Generic)

newtype Source a = Source {rawSource :: TChan a}
  deriving (Eq, Generic)

newSink :: STM (Sink a)
newSink = Sink <$> newBroadcastTChan

newSinkIO :: (Concurrent :> es) => Eff es (Sink a)
newSinkIO = Sink <$> newBroadcastTChanIO

writeSink :: Sink a -> a -> STM ()
writeSink (Sink chan) a = writeTChan chan a

readSource :: Source a -> STM a
readSource (Source chan) = readTChan chan

subscribeSink :: Sink a -> STM (Source a)
subscribeSink (Sink chan) = Source <$> dupTChan chan
