module SequentPlugin (plugin) where

import GhcPlugins ( Plugin(installCoreToDos), CommandLineOption
                  , defaultPlugin
                  , reinitializeGlobals
                  , CoreM, CoreToDo(CoreDoPluginPass)
                  , isBottomingId, idArity, isTypeArg
                  , putMsg
                  , Var
                  )
import SequentCore

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install
}

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install opts todos =
  do reinitializeGlobals
     return $ dump : todos ++ [dump]
  where dump = CoreDoPluginPass "sequent-core-no-op" (sequentPass showSequentCore)

showSequentCore :: [Bind Var] -> CoreM [Bind Var]
showSequentCore bs = do
  putMsg (ppr_binds_top bs)
  return bs

-- | Inline the continuation of a case into all of its branches.
inlineCaseCont :: Command b -> Maybe (Command b)
inlineCaseCont cmd =
  case cmdCont cmd of
    Case x t as : f : more ->
      Just cmd { cmdCont = Case x t (map inline as) : more }
      where
      inline (Alt ac xs c) = Alt ac xs (c { cmdCont = cmdCont c ++ [f] })

    _ -> Nothing


-- | Does this look like bottom
-- (i.e., a known bottom function applied to enough arguments)
commandIsBottom :: Command b -> Bool
commandIsBottom cmd =
  case cmdValue cmd of
    Var v -> isBottomingId v && contArgs (cmdCont cmd) >= idArity v
    _     -> False

contArgs = undefined
{-
-- | How many immediatly visible arguments we have in a continuation.
contArgs :: [Frame b] -> Int
contArgs fs = length [ () | App a <- takeWhile notCase fs, not (isTypeArg a) ]
  where notCase (Case {}) = False
        notCase _         = True


-}


