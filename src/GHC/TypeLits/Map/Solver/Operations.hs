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
  | KeysOp MapOp
  | ElemsOp MapOp
  | AssocsOp MapOp
  | CastOp MapOp GHC.KindCoercion
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
        KeysOp mop ->
          GHC.text "KeysOp" <+> GHC.ppr mop
        ElemsOp mop ->
          GHC.text "ElemsOp" <+> GHC.ppr mop
        AssocsOp mop ->
          GHC.text "AssocsOp" <+> GHC.ppr mop
        CastOp op crc ->
          GHC.text "CastOp" <+> GHC.ppr op <+> GHC.ppr crc
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
    = Coerce.coerce GHC.nonDetCmpType

solvedOpsEqPred
  :: (GHC.Ct, (MapOp, MapOp))
  -> PluginM ((GHC.EvTerm, GHC.Ct), GHC.Ct)

solvedOpsEqPred (ct, (op1, op2)) = do
  t1 <- mapOpToType op1
  t2 <- mapOpToType op2
  w <- Trans.lift $ GHC.newWanted (GHC.ctLoc ct) (GHC.mkPrimEqPred t1 t2)
  let wct    = GHC.mkNonCanonical w
      result = ((GHC.evByFiat Meta.packageName (GHC.ctPred ct) t1, ct), wct)
  pure result

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
          LookupAllOp (_kk, _kts) _mop -> do
            error "mapOpToType: LookupAllOp"
          FromListOp ((kk, vk), m) -> do
            pluginTrace "mapOpToType: FromList" (kk, vk, m)
            let as     = M.assocs m
                ppair  = GHC.promotedTupleDataCon GHC.Boxed 2
                mkPair = \(OrdType kt, vt) -> GHC.mkTyConApp ppair [kk, vk, kt, vt]
                ast    = GHC.mkPromotedListTy (GHC.mkTyConApp ppair [kk, vk]) (mkPair <$> as)
            pure (GHC.mkTyConApp _peFromListTyCon [kk, vk, ast])
          KeysOp mop -> do
            pluginTrace "mapOpToType: KeysOp" mop
            mt <- mapOpToType mop
            pure (GHC.mkTyConApp _peKeysTyCon [mt])
          ElemsOp mop -> do
            pluginTrace "mapOpToType: ElemsOp" mop
            mt <- mapOpToType mop
            pure (GHC.mkTyConApp _peElemsTyCon [mt])
          AssocsOp mop -> do
            pluginTrace "mapOpToType: AssocsOp" mop
            mt <- mapOpToType mop
            pure (GHC.mkTyConApp _peAssocsTyCon [mt])
          CastOp op' crc -> do
            pluginTrace "mapOpToType: CastOp" (op', crc)
            t <- mapOpToType op'
            pure (GHC.mkCastTy t crc)
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
        let simplify = GHC.expandTypeSynonyms . GHC.substTy sub
        op1 <- typeToMapOp (simplify t1)
        op2 <- typeToMapOp (simplify t2)
        pluginTrace "eqPredMapOps: EqPred/post-substitution" (op1, op2)
        pure $ case (peelOffCasts op1, peelOffCasts op2) of
          (TypeOp _, TypeOp _) ->
            Nothing
          ops ->
            Just (ct, ops)
      _ ->
        pure Nothing

peelOffCasts :: MapOp -> MapOp
peelOffCasts
  = go
  where
    go op
      = case op of
          CastOp op' _ ->
            go op'
          _ ->
            op

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
          GHC.TyConApp tc [_kk, _vk, mt]
            | tc == _peKeysTyCon -> do
                pluginTrace "typeToMapOp: Keys" mt
                mop <- go mt
                pure (KeysOp mop)
          GHC.TyConApp tc [_kk, _vk, mt]
            | tc == _peElemsTyCon -> do
                pluginTrace "typeToMapOp: Elems" mt
                mop <- go mt
                pure (ElemsOp mop)
          GHC.TyConApp tc [_kk, _vk, mt]
            | tc == _peAssocsTyCon -> do
                pluginTrace "typeToMapOp: Assocs" mt
                mop <- go mt
                pure (AssocsOp mop)
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
          GHC.CastTy t crc -> do
            pluginTrace "typeToMapOp: Coercion" (t, crc)
            op <- go t
            pure (CastOp op crc)
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
