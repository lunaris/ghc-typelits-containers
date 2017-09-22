{-# LANGUAGE LambdaCase #-}

module GHC.TypeLits.Map.Solver.Reduce where

import           GHC.TypeLits.Map.Solver.GHC        as GHC
import           GHC.TypeLits.Map.Solver.Operations

import qualified Control.Monad.State.Strict         as St
import qualified Data.Map.Strict                    as M
import           Data.Maybe                         (fromMaybe)

type ReduceM
  = St.State ReductionResult

data ReductionResult
  = Unchanged
  | Reduced

reduceBy :: ReduceM a -> ReduceM a
reduceBy m
  = St.put Reduced *> m

reduceOps :: (GHC.Ct, (MapOp, MapOp)) -> Maybe (GHC.Ct, (MapOp, MapOp))
reduceOps (ct, (op1, op2))
  = case (reduceOp op1, reduceOp op2) of
      (Nothing, Nothing) ->
        Nothing
      (maybeOp1', maybeOp2') ->
        Just (ct, (fromMaybe op1 maybeOp1', fromMaybe op2 maybeOp2'))

reduceOp :: MapOp -> Maybe MapOp
reduceOp
  = nothingIfUnchanged . flip St.runState Unchanged . go
  where
    nothingIfUnchanged
      = \case
          (_, Unchanged) ->
            Nothing
          (x, Reduced) ->
            Just x

    go :: MapOp -> ReduceM MapOp
    go op
      = case op of
          LookupOp kop mop -> do
            kop' <- go kop
            mop' <- go mop
            case (kop', mop') of
              (TypeOp kt, FromListOp ((_kk, vk), m)) -> reduceBy $ do
                case M.lookup (OrdType kt) m of
                  Nothing ->
                    pure $ TypeOp $ GHC.mkTyConApp GHC.promotedNothingDataCon [vk]
                  Just vt ->
                    pure $ TypeOp $ GHC.mkTyConApp GHC.promotedJustDataCon [vk, vt]
              _ ->
                pure (LookupOp kop' mop')
          LookupAllOp (_kk1, ksl) mop -> do
            mop' <- go mop
            case mop' of
              FromListOp ((_kk2, vk), m) -> reduceBy $ do
                case traverse (\kt -> M.lookup (OrdType kt) m) ksl of
                  Nothing ->
                    pure $ TypeOp $ GHC.mkTyConApp GHC.promotedNothingDataCon [vk]
                  Just vts ->
                    pure $ TypeOp $ GHC.mkTyConApp GHC.promotedJustDataCon
                      [ GHC.mkTyConApp GHC.listTyCon [vk]
                      , GHC.mkPromotedListTy vk vts
                      ]
              _ ->
                pure (LookupAllOp (_kk1, ksl) mop')
          _ -> do
            pure op
