{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards            #-}

module GHC.TypeLits.Map.Solver.Operations where

import           GHC.TypeLits.Map.Solver.Env
import qualified GHC.TypeLits.Map.Solver.GHC  as GHC
import           GHC.TypeLits.Map.Solver.GHC  ((<+>))
import qualified GHC.TypeLits.Map.Solver.Meta as Meta

import qualified Control.Monad.Reader         as Rdr
import qualified Control.Monad.Trans          as Trans
import qualified Data.Coerce                  as Coerce
import qualified Data.Map.Strict              as M

data MapOp
  = LookupOp MapOp MapOp
  | LookupAllOp (GHC.Kind, [GHC.Type]) MapOp
  | FromListOp ((GHC.Kind, GHC.Kind), M.Map OrdType GHC.Type)
  | TypeOp GHC.Type

instance GHC.Outputable MapOp where
  ppr
    = GHC.parens . \case
        LookupOp kop mop ->
          GHC.text "LookupOp" <+> GHC.ppr kop <+> GHC.ppr mop
        LookupAllOp ks mop ->
          GHC.text "LookupAllOp" <+> GHC.ppr ks <+> GHC.ppr mop
        FromListOp as ->
          GHC.text "FromListOp" <+> GHC.ppr as
        TypeOp ty ->
          GHC.text "TypeOp" <+> GHC.ppr ty

newtype OrdType
  = OrdType GHC.Type
  deriving (GHC.Outputable)

instance Eq OrdType where
  (==)
    = Coerce.coerce GHC.eqType

instance Ord OrdType where
  compare
    = Coerce.coerce GHC.cmpType

solvedOpsEqPred
  :: (GHC.Ct, (Maybe MapOp, Maybe MapOp))
  -> PluginM (Maybe ((GHC.EvTerm, GHC.Ct), GHC.Ct))

solvedOpsEqPred (ct, (maybeOp1, maybeOp2)) = do
  maybeT1 <- traverse mapOpToType maybeOp1
  maybeT2 <- traverse mapOpToType maybeOp2
  let k t1 t2 = Trans.lift $ do
        w <- GHC.newWanted (GHC.ctLoc ct) (GHC.mkPrimEqPred t1 t2)
        let wct = GHC.mkNonCanonical w
        pure ((GHC.evByFiat Meta.packageName (GHC.ctPred ct) t1, ct), wct)
  sequence (k <$> maybeT1 <*> maybeT2)

mapOpToType :: MapOp -> PluginM GHC.Type
mapOpToType
  = go
  where
    go op
      = Rdr.ask >>= \PluginEnv{..} -> case op of
          LookupOp kop mop -> do
            pluginTrace "mapOpToType: LookupOp" (kop, mop)
            kt <- mapOpToType kop
            mt <- mapOpToType mop
            pure (GHC.mkTyConApp _peLookupTyCon [kt, mt])
          LookupAllOp _ksl _mop -> do
            error "mapOpToType: LookupAllOp"
          FromListOp _m -> do
            error "mapOpToType: FromListOp"
          TypeOp ty ->
            pure ty

eqPredMapOps
  :: GHC.TCvSubst
  -> GHC.Ct
  -> PluginM (Maybe (GHC.Ct, (MapOp, MapOp)))

eqPredMapOps sub ct
  = case GHC.classifyPredType (GHC.ctEvPred (GHC.ctEvidence ct)) of
      GHC.EqPred GHC.NomEq t1 t2 -> do
        pluginTrace "eqPredMapOps: EqPred/pre-substitution" (t1, t2)
        let applySub = GHC.substTy sub
        op1 <- typeToMapOp (applySub t1)
        op2 <- typeToMapOp (applySub t2)
        pluginTrace "eqPredMapOps: EqPred/post-substitution" (op1, op2)
        pure $ case (op1, op2) of
          (TypeOp _, TypeOp _) ->
            Nothing
          ops ->
            Just (ct, ops)
      _ ->
        pure Nothing

typeToMapOp :: GHC.Type -> PluginM MapOp
typeToMapOp
  = go
  where
    go :: GHC.Type -> PluginM MapOp
    go ty
      = Rdr.ask >>= \PluginEnv{..} -> case ty of
          GHC.TyConApp tc [_kk, _vk, ast]
            | tc == _peFromListTyCon -> do
                pluginTrace "typeToMapOp: FromList" ast
                let mm = tryExtractMap ast
                pure (maybe (TypeOp ty) FromListOp mm)
          GHC.TyConApp tc [_kk, _vk, kt, mt]
            | tc == _peLookupTyCon -> do
                pluginTrace "typeToMapOp: Lookup" (kt, mt)
                kop <- go kt
                mop <- go mt
                pure (LookupOp kop mop)
          GHC.TyConApp tc [_ksk, _vk, kst, mt]
            | tc == _peLookupAllTyCon -> do
                pluginTrace "typeToMapOp: LookupAll" (kst, mt)
                let mksl = GHC.tryExtractPromotedList kst
                mop <- go mt
                pure (maybe (TypeOp ty) (\ksl -> LookupAllOp ksl mop) mksl)
          _ ->
            pure $ TypeOp ty

tryExtractMap
  :: GHC.Type
  -> Maybe ((GHC.Kind, GHC.Kind), M.Map OrdType GHC.Type)

tryExtractMap
  = fmap (fmap M.fromList) . go
  where
    go
      = \case
          GHC.TyConApp tc1 [GHC.TyConApp _tc2 [kk, vk]]
            | tc1 == GHC.promotedNilDataCon ->
                Just ((kk, vk), [])
          GHC.TyConApp tc1 [_ak, GHC.TyConApp tc2 [_kk, _vk, kt, vt], ast]
            | tc1 == GHC.promotedConsDataCon &&
              tc2 == GHC.promotedTupleDataCon GHC.Boxed 2 ->
                fmap ((OrdType kt, vt) :) <$> go ast
          _ ->
            Nothing