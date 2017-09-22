module GHC.TypeLits.Map.Solver.Env where

import qualified GHC.TypeLits.Map.Solver.Meta as Meta
import qualified GHC.TypeLits.Map.Solver.GHC  as GHC

import qualified Control.Monad.Reader         as Rdr
import qualified Control.Monad.Trans          as Trans

type PluginM
  = Rdr.ReaderT PluginEnv GHC.TcPluginM

data PluginEnv
  = PluginEnv
      { _peMapTyCon       :: GHC.TyCon
      , _peFromListTyCon  :: GHC.TyCon
      , _peLookupTyCon    :: GHC.TyCon
      , _peLookupAllTyCon :: GHC.TyCon
      }

runPluginM :: PluginEnv -> PluginM a -> GHC.TcPluginM a
runPluginM env m
  = Rdr.runReaderT m env

pluginTrace :: GHC.Outputable a => String -> a -> PluginM ()
pluginTrace msg x
  = Trans.lift $ GHC.tcPluginTrace (format msg) (GHC.ppr x)
  where
    format s = '[' : Meta.packageName ++ "] " ++ s ++ ": "
