module Main (main) where

foreign export javascript "hs_start" main :: IO ()

main :: IO ()
main = pure ()
