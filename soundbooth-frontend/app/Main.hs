module Main (main) where

import qualified Language.Javascript.JSaddle.Runner as JS
import Soundbooth.Frontend (defaultMain)

foreign export javascript "hs_start" main :: IO ()

main :: IO ()
main = JS.run defaultMain
