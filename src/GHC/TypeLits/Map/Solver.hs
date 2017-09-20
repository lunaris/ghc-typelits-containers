module GHC.TypeLits.Map.Solver
  ( plugin
  ) where

--import Control.Monad.Reader
--import Data.Maybe
--import DataCon
--import FastString
import GHC.TcPluginM.Extra
import GhcPlugins           hiding (substTy)
--import TcEvidence
import TcPluginM            hiding (newWanted)
import TcRnTypes
--import TyCoRep
--import Unify

plugin :: Plugin
plugin
  = defaultPlugin
      { tcPlugin = const (Just plugin')
      }

plugin' :: TcPlugin
plugin'
  = tracePlugin "ghc-typelits-containers:Map" TcPlugin
      { tcPluginInit  = pure ()
      , tcPluginSolve = pluginSolve
      , tcPluginStop  = const (pure ())
      }

pluginSolve :: () -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
pluginSolve _ _gs [] []
  = pure (TcPluginOk [] [])
pluginSolve _ _gs _ds _ws = do
  pure (TcPluginOk [] [])
