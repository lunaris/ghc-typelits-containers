{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module GHC.TypeLits.Map
  ( Map

  , HasKey
  , HasKeys
  , AllValues
  , withAllValues

  , FromList
  , Lookup
  , LookupAll
  ) where

import Data.Proxy
import GHC.Exts
--import GHC.TypeLits

data Map key value

class HasKey (m :: Map key value) (k :: key) (v :: value) | m k -> v

instance (Lookup k m ~ 'Just v) => HasKey (m :: Map key value) (k :: key) (v :: value)

class HasKeys (m :: Map key value) (ks :: [key]) (vs :: [value]) | m ks -> vs

instance (LookupAll ks m ~ 'Just vs) => HasKeys (m :: Map key value) (ks :: [key]) (vs :: [value])

class AllValues (c :: value -> Constraint) (m :: Map key value) where

withAllValues
  :: (AllValues c m,
      Monoid z)
  => (forall x. c x => Proxy x -> z)
  -> z

withAllValues _k
  = undefined

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

type family LookupAll (ks :: [key]) (m :: Map key value) :: Maybe [value]

type family FromJust (ma :: Maybe a) :: a where
  FromJust ('Just a)
    = a
