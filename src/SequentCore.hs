module SequentCore where

import GhcPlugins ( CoreM
                  , Id, Literal, Type, Coercion, AltCon, Tickish, Var
                  , ModGuts
                  , bindsOnlyPass
                  , deShadowBinds
                  )
import qualified GhcPlugins as GHC


sequentPass :: ([Bind Var] -> CoreM [Bind Var]) -> ModGuts -> CoreM ModGuts
sequentPass process =
  bindsOnlyPass (fmap toCoreBinds . process . fromCoreBinds . deShadowBinds)


--------------------------------------------------------------------------------
-- Sequent Calculus Bases AST
--------------------------------------------------------------------------------


data Value b    = Lit Literal
                | Lam b (Command b)
                | Type Type
                | Coercion Coercion
                | Var Id

data Frame b    = App  {- expr -} (Command b)
                | Case {- expr -} b Type [Alt b]
                | Cast {- expr -} Coercion
                | Tick (Tickish Id) {- expr -}

data Command b  = Command { cmdLet   :: [Bind b]
                          , cmdValue :: Value b
                          , cmdCont  :: [Frame b]
                          }

data Bind b     = NonRec b (Command b)
                | Rec [(b, Command b)]

data Alt b      = Alt AltCon [b] (Command b)


fromCoreExpr :: GHC.Expr b -> Command b
fromCoreExpr = go [] []
  where
  val binds frames v =
    Command { cmdLet = binds, cmdCont = frames, cmdValue = v }

  go binds frames expr =
    case expr of
      GHC.Var x         -> val binds frames (Var x)
      GHC.Lit l         -> val binds frames (Lit l)
      GHC.App e1 e2     -> go binds (App (fromCoreExpr e2) : frames) e1
      GHC.Lam x e       -> val binds frames (Lam x (fromCoreExpr e))
      GHC.Let bs e      -> go (fromCoreBind bs : binds) frames e
      GHC.Case e x t as -> go binds (Case x t (map fromCoreAlt as) : frames) e
      GHC.Cast e co     -> go binds (Cast co : frames) e
      GHC.Tick ti e     -> go binds (Tick ti : frames) e
      GHC.Type t        -> val binds frames (Type t)
      GHC.Coercion co   -> val binds frames (Coercion co)

fromCoreAlt :: GHC.Alt b -> Alt b
fromCoreAlt (ac, bs, e) = Alt ac bs (fromCoreExpr e)

fromCoreBind :: GHC.Bind b -> Bind b
fromCoreBind bind =
  case bind of
    GHC.NonRec b e -> NonRec b (fromCoreExpr e)
    GHC.Rec bs     -> Rec [ (b, fromCoreExpr e) | (b,e) <- bs ]

fromCoreBinds :: [GHC.Bind b] -> [Bind b]
fromCoreBinds = map fromCoreBind

commandToCoreExpr :: Command b -> GHC.Expr b
commandToCoreExpr cmd = foldl addLet baseExpr (cmdLet cmd)
  where
  addLet e b  = GHC.Let (toCoreBind b) e
  baseExpr    = foldl (flip frameToCoreExpr)
                      (valueToCoreExpr (cmdValue cmd))
                      (cmdCont cmd)

valueToCoreExpr :: Value b -> GHC.Expr b
valueToCoreExpr val =
  case val of
    Lit l       -> GHC.Lit l
    Lam b c     -> GHC.Lam b (commandToCoreExpr c)
    Type t      -> GHC.Type t
    Coercion co -> GHC.Coercion co
    Var x       -> GHC.Var x

frameToCoreExpr :: Frame b -> GHC.Expr b -> GHC.Expr b
frameToCoreExpr frame e =
  case frame of
    App  {- expr -} e2      -> GHC.App e (commandToCoreExpr e2)
    Case {- expr -} b t as  -> GHC.Case e b t (map toCoreAlt as)
    Cast {- expr -} co      -> GHC.Cast e co
    Tick ti {- expr -}      -> GHC.Tick ti e

toCoreBind :: Bind b -> GHC.Bind b
toCoreBind bind =
  case bind of
    NonRec b c -> GHC.NonRec b (commandToCoreExpr c)
    Rec bs     -> GHC.Rec [ (b,commandToCoreExpr c) | (b,c) <- bs ]

toCoreBinds :: [Bind b] -> [GHC.Bind b]
toCoreBinds = map toCoreBind

toCoreAlt :: Alt b -> GHC.Alt b
toCoreAlt (Alt ac bs c) = (ac, bs, commandToCoreExpr c)




