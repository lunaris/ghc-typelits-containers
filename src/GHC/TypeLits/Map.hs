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

  , FromList
  , Lookup
  , LookupAll
  , Keys
  , Elems
  , Assocs
  ) where

data Map key value

class HasKey (m :: Map key value) (k :: key) (v :: value) | m k -> v

instance (Lookup k m ~ 'Just v)
      =>  HasKey (m :: Map key value) (k :: key) (v :: value)

class HasKeys (m :: Map key value) (ks :: [key]) (vs :: [value]) | m ks -> vs

instance (LookupAll ks m ~ 'Just vs)
      =>  HasKeys (m :: Map key value) (ks :: [key]) (vs :: [value])

type family FromList (as :: [(key, value)]) :: Map key value

type family Lookup (k :: key) (m :: Map key value) :: Maybe value

type family LookupAll (ks :: [key]) (m :: Map key value) :: Maybe [value]

type family Keys (m :: Map key value) :: [key]

type family Elems (m :: Map key value) :: [value]

type family Assocs (m :: Map key value) :: [(key, value)]

type family FromJust (ma :: Maybe a) :: a where
  FromJust ('Just a)
    = a
