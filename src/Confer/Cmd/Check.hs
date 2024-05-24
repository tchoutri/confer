module Confer.Cmd.Check (check) where

import Effectful

import Confer.Effect.Symlink
import Confer.Config.Evaluator

check :: (IOE :> es, Symlink :> es) => Eff es ()
check = do
  loadConfiguration >>= liftIO . print
