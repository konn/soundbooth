{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- A port of proteaaudio to effectful
module Effectful.Audio (
  Audio,
  AudioOptions (..),
  runAudio,
  volume,
  soundActiveAll,
  getNumberOfActiveSounds,
  soundStopAll,
  stopAll,
  loaderAvailable,
  Sound,
  Sample,
  sampleFromMemoryPcm,
  sampleFromMemoryWav,
  sampleFromMemoryOgg,
  sampleFromMemoryMp3,
  sampleFromMemory,
  sampleFromFile,
  sampleDestroy,
  destroy,
  soundLoop,
  loop,
  soundPlay,
  play,
  soundPlayOn,
  playOn,
  soundLoopOn,
  loopOn,
  soundUpdate,
  update,
  soundStop,
  stop,
  soundActive,
  isActive,
) where

import Control.Exception.Safe
import Control.Monad (unless)
import Data.ByteString qualified as BS
import Effectful
import Effectful.Dispatch.Static
import GHC.Generics (Generic)
import Sound.ProteaAudio (Sample, Sound)
import Sound.ProteaAudio qualified as P

data Audio :: Effect

data AudioOptions = AudioOptions
  { numTracks :: !Int
  , sampleFreq :: !Int
  , bucketSize :: !Int
  }
  deriving (Show, Eq, Ord, Generic)

type instance DispatchOf Audio = 'Static 'WithSideEffects

data instance StaticRep Audio = AudioRep

initAudio' :: AudioOptions -> IO ()
initAudio' aopts =
  (`unless` throwString "Audio initialisation failed!")
    =<< P.initAudio aopts.numTracks aopts.sampleFreq aopts.bucketSize

runAudio :: (IOE :> es) => AudioOptions -> Eff (Audio ': es) a -> Eff es a
runAudio opts =
  evalStaticRep AudioRep
    . bracket_ (liftIO $ initAudio' opts) (liftIO P.finishAudio)

volume :: (Audio :> es) => Float -> Float -> Eff es ()
volume = fmap unsafeEff_ . P.volume

-- | A synonym of 'soundActiveAll'
getNumberOfActiveSounds :: (Audio :> es) => Eff es Int
getNumberOfActiveSounds = soundActiveAll

soundActiveAll :: (Audio :> es) => Eff es Int
soundActiveAll = unsafeEff_ P.soundActiveAll

soundStopAll :: (Audio :> es) => Eff es ()
soundStopAll = unsafeEff_ P.soundStopAll

-- | A synonym of 'soundStopAll'
stopAll :: (Audio :> es) => Eff es ()
stopAll = soundStopAll

loaderAvailable :: (Audio :> es) => String -> Eff es Bool
loaderAvailable = unsafeEff_ . P.loaderAvailable

sampleFromMemoryPcm :: BS.ByteString -> Int -> Int -> Int -> Float -> Eff es Sample
sampleFromMemoryPcm mem chans rate bps =
  unsafeEff_ . P.sampleFromMemoryPcm mem chans rate bps

sampleFromMemoryWav :: BS.ByteString -> Float -> Eff es Sample
sampleFromMemoryWav mem = unsafeEff_ . P.sampleFromMemoryWav mem

sampleFromMemoryOgg :: BS.ByteString -> Float -> Eff es Sample
sampleFromMemoryOgg mem = unsafeEff_ . P.sampleFromMemoryOgg mem

sampleFromMemoryMp3 :: BS.ByteString -> Float -> Eff es Sample
sampleFromMemoryMp3 mem = unsafeEff_ . P.sampleFromMemoryMp3 mem

sampleFromMemory :: BS.ByteString -> Float -> Eff es Sample
sampleFromMemory mem = unsafeEff_ . P.sampleFromMemory mem

sampleFromFile :: FilePath -> Float -> Eff es Sample
sampleFromFile path = unsafeEff_ . P.sampleFromFile path

sampleDestroy :: (Audio :> es) => Sample -> Eff es Bool
sampleDestroy = unsafeEff_ . P.sampleDestroy

-- | A synonym of 'sampleDestroy'
destroy :: (Audio :> es) => Sample -> Eff es Bool
destroy = sampleDestroy

soundLoop :: (Audio :> es) => Sample -> Float -> Float -> Float -> Float -> Eff es Sound
soundLoop = fmap (fmap $ fmap $ fmap unsafeEff_) . P.soundLoop

-- | A synonym of 'soundLoop'
loop :: (Audio :> es) => Sample -> Float -> Float -> Float -> Float -> Eff es Sound
loop = soundLoop

soundPlay :: (Audio :> es) => Sample -> Float -> Float -> Float -> Float -> Eff es Sound
soundPlay = fmap (fmap $ fmap $ fmap unsafeEff_) . P.soundPlay

-- | A synonym of 'soundPlay'
play :: (Audio :> es) => Sample -> Float -> Float -> Float -> Float -> Eff es Sound
play = soundPlay

soundPlayOn :: (Audio :> es) => Int -> Sample -> Float -> Float -> Float -> Float -> Eff es Sound
soundPlayOn = fmap (fmap $ fmap $ fmap $ fmap unsafeEff_) . P.soundPlayOn

-- | A synonym of 'soundPlayOn'
playOn :: (Audio :> es) => Int -> Sample -> Float -> Float -> Float -> Float -> Eff es Sound
playOn = soundPlayOn

soundLoopOn :: (Audio :> es) => Int -> Sample -> Float -> Float -> Float -> Float -> Eff es Sound
soundLoopOn = fmap (fmap $ fmap $ fmap $ fmap unsafeEff_) . P.soundLoopOn

-- | A synonym of 'soundLoopOn'
loopOn :: (Audio :> es) => Int -> Sample -> Float -> Float -> Float -> Float -> Eff es Sound
loopOn = soundLoopOn

soundUpdate :: (Audio :> es) => Sound -> Bool -> Float -> Float -> Float -> Float -> Eff es Bool
soundUpdate = fmap (fmap $ fmap $ fmap $ fmap unsafeEff_) . P.soundUpdate

-- | A synonym of 'soundUpdate'
update :: (Audio :> es) => Sound -> Bool -> Float -> Float -> Float -> Float -> Eff es Bool
update = soundUpdate

soundStop :: (Audio :> es) => Sound -> Eff es Bool
soundStop = unsafeEff_ . P.soundStop

-- | A synonym of 'soundStop'
stop :: (Audio :> es) => Sound -> Eff es Bool
stop = soundStop

soundActive :: (Audio :> es) => Sound -> Eff es Bool
soundActive = unsafeEff_ . P.soundActive

-- | A synonym of 'soundActive'
isActive :: (Audio :> es) => Sound -> Eff es Bool
isActive = soundActive
