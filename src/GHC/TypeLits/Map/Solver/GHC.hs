{-# LANGUAGE LambdaCase #-}

module GHC.TypeLits.Map.Solver.GHC
  ( module Exports

  , module GHC.TypeLits.Map.Solver.GHC
  ) where

import Data.Maybe          (fromMaybe)
import FastString          as Exports
import GHC.TcPluginM.Extra as Exports (evByFiat, lookupModule, lookupName, tracePlugin)
import GhcPlugins          as Exports hiding (extendTvSubstList, isInScope, substCo, substTy)
import TcEvidence          as Exports
import TcPluginM           as Exports
import TcRnTypes           as Exports
import TcType              as Exports
import TyCoRep             as Exports
import Unify               as Exports

collectEqCtSubsts :: [Ct] -> TCvSubst
collectEqCtSubsts
  = foldr (unionTCvSubst . k) emptyTCvSubst
  where
    k ct
      = case classifyPredType (ctPred ct) of
          EqPred NomEq t1 t2 ->
            fromMaybe emptyTCvSubst (tcUnifyTy t1 t2)
          _ ->
            emptyTCvSubst

mkPromotedListTy :: Kind -> [Type] -> Type
mkPromotedListTy k
  = foldr cons nil
  where
    cons t ts = mkTyConApp promotedConsDataCon [k, t, ts]
    nil       = mkTyConApp promotedNilDataCon [k]

tryExtractPromotedList :: Type -> Maybe (Kind, [Type])
tryExtractPromotedList
  = go
  where
    go
      = \case
          TyConApp tc [k]
            | tc == promotedNilDataCon ->
                Just (k, [])
          TyConApp tc [_k, t, ts]
            | tc == promotedConsDataCon ->
                fmap (t :) <$> go ts
          _ ->
            Nothing
