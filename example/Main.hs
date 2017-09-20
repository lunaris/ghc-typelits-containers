{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Map.Solver #-}

module Main (main) where

import Data.Proxy
import GHC.TypeLits.Map

testFromList :: Proxy (FromList '[ '("A", 1), '("B", 2)])
testFromList
  = Proxy

main :: IO ()
main
  = pure ()
