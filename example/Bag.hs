{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeInType            #-}

{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Bag where

import qualified Data.Aeson            as Ae
import           Data.Dynamic
import           Data.Functor.Identity
import           Data.Kind
import qualified Data.Map.Strict       as M
import           Data.Proxy
import qualified Data.Text             as Tx
import           GHC.TypeLits
import qualified GHC.TypeLits.Map      as TM
import           Type.Reflection

newtype Bag (f :: Type -> Type) (cs :: [Type -> Constraint]) (m :: TM.Map Symbol Type)
  = Bag { _bagMap :: M.Map Tx.Text (Dictamic cs) }

data Dictamic :: [Type -> Constraint] -> Type where
  Dictamic :: ApplyAll cs a => TypeRep a -> a -> Dictamic cs

data AllDicts :: [Type -> Constraint] -> Type -> Type where
  AllDicts :: ApplyAll cs a => AllDicts cs a

type family ApplyAll (cs :: [Type -> Constraint]) (a :: Type) :: Constraint where
  ApplyAll (c ': cs) a = (c a, ApplyAll cs a)
  ApplyAll '[]       a = ()

instance Monoid (Bag f cs m) where
  mempty
    = Bag mempty
  mappend (Bag m1) (Bag m2)
    = Bag (m1 `M.union` m2)

insert
  :: forall k v f cs m.
     (KnownSymbol k,
      Typeable (f v),
      TM.HasKey m k v,
      ApplyAll cs (f v))
  => f v
  -> Bag f cs m
  -> Bag f cs m

insert fv (Bag m)
  = Bag (M.insert (symbolText @k) (toDictamic fv) m)

insertValue
  :: forall k v cs m.
     (KnownSymbol k,
      Typeable v,
      TM.HasKey m k v,
      ApplyAll cs (Identity v))
  => v
  -> Bag Identity cs m
  -> Bag Identity cs m

insertValue
  = insert @k . Identity

lookup
  :: forall k v f cs m.
     (KnownSymbol k,
      Typeable (f v),
      TM.HasKey m k v)
  => Bag f cs m
  -> Maybe (f v)

lookup (Bag m)
  = M.lookup (symbolText @k) m >>= fromDictamic

symbolText :: forall s. KnownSymbol s => Tx.Text
symbolText
  = Tx.pack (symbolVal (Proxy @s))

toDictamic
  :: forall cs a.
     (ApplyAll cs a,
      Typeable a)
  => a
  -> Dictamic cs

toDictamic
  = Dictamic (typeRep @a)

fromDictamic
  :: forall cs a.
     Typeable a
  => Dictamic cs
  -> Maybe a

fromDictamic (Dictamic rep x)
  | Just HRefl <- rep `eqTypeRep` typeRep @a = Just x
  | otherwise                                = Nothing

bagToJSON
  :: forall f cs m.
     Implies cs Ae.ToJSON
  => Bag f cs m
  -> Ae.Value

bagToJSON (Bag m)
  = Ae.toJSON $ fmap k m
  where
    k :: Dictamic cs -> Ae.Value
    k (Dictamic _ fa)
      = weaken @cs @Ae.ToJSON Ae.toJSON fa

class Implies (cs :: [Type -> Constraint]) (c :: Type -> Constraint) where
  weaken :: forall a r. ApplyAll cs a => (c a => a -> r) -> a -> r

instance Implies (c ': cs) c where
  weaken k x
    = k x
