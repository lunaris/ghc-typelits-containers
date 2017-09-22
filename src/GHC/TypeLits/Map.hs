{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module GHC.TypeLits.Map
  ( Map
  , FromList
  , Lookup
  ) where

--import GHC.TypeLits

data Map key value

type family FromList (as :: [(key, value)]) :: Map key value {-where
  FromList as
    = TypeError
        (     'Text "The FromList type family cannot be reduced without the ghc-typelits-containers type checker plugin."
        ':$$: 'Text "You can enable this plugin in a given module with an OPTIONS_GHC pragma as follows:"
        ':$$: 'Text ""
        ':$$: 'Text "  {-# OPTIONS_GHC -fplugin GHC.TypeLits.Map.Solver #-}"
        ':$$: 'Text ""
        ':$$: 'Text "Alternatively, you can pass the -fplugin GHC.TypeLits.Map.Solver option to GHC directly."
        )-}

type family Lookup (k :: key) (m :: Map key value) :: Maybe value {-where
  Lookup k m
    = TypeError
        (     'Text "The Lookup type family cannot be reduced without the ghc-typelits-containers type checker plugin."
        ':$$: 'Text "You can enable this plugin in a given module with an OPTIONS_GHC pragma as follows:"
        ':$$: 'Text ""
        ':$$: 'Text "  {-# OPTIONS_GHC -fplugin GHC.TypeLits.Map.Solver #-}"
        ':$$: 'Text ""
        ':$$: 'Text "Alternatively, you can pass the -fplugin GHC.TypeLits.Map.Solver option to GHC directly."
        )-}
