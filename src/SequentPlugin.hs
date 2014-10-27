module SequentPlugin (plugin) where

import GhcPlugins
import SequentCore

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install
}

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install opts todos =
  do reinitializeGlobals
     return (CoreDoPluginPass "sequent-core-no-op" (sequentPass return) : todos)




