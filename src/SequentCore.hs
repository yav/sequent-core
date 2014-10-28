module SequentCore where

import GhcPlugins ( CoreM
                  , Id, Literal, Type, Coercion, AltCon, Tickish, Var
                  , ModGuts
                  , bindsOnlyPass
                  , deShadowBinds
                  , sLit, pprLiteral
                  )
import qualified GhcPlugins as GHC
import Outputable

import Data.List

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

ppr_bind :: OutputableBndr b => Bind b -> SDoc
ppr_bind (NonRec val_bdr expr) = ppr_binding (val_bdr, expr)
ppr_bind (Rec binds)           = hang (text "rec") 2 (vcat $ intersperse space $ ppr_block "{" ";" "}" (map ppr_binding binds))

ppr_binds_top :: OutputableBndr b => [Bind b] -> SDoc
ppr_binds_top binds = ppr_binds_with "" "" "" binds

ppr_block :: String -> String -> String -> [SDoc] -> [SDoc]
ppr_block open mid close [] = [text open <> text close]
ppr_block open mid close (first : rest)
  = text open <+> first : map (text mid <+>) rest ++ [text close]

ppr_binds :: OutputableBndr b => [Bind b] -> SDoc
ppr_binds binds = ppr_binds_with "{" ";" "}" binds

ppr_binds_with :: OutputableBndr b => String -> String -> String -> [Bind b] -> SDoc
ppr_binds_with open mid close binds = vcat $ intersperse space $ ppr_block open mid close (map ppr_bind binds)

ppr_binding :: OutputableBndr b => (b, Command b) -> SDoc
ppr_binding (val_bdr, expr)
  = pprBndr LetBind val_bdr $$
    hang (ppr val_bdr <+> equals) 2 (pprCoreComm expr)

ppr_comm :: OutputableBndr b => (SDoc -> SDoc) -> Command b -> SDoc
ppr_comm add_par comm
  = maybe_add_par $ ppr_let <+> cut (cmdValue comm) (cmdCont comm)
  where
    ppr_let
      = case cmdLet comm of
          [] -> empty
          binds -> hang (text "let") 2 (ppr_binds binds) $$ text "in"
      {-
      = hang (ptext keyword) 2 (ppr_bind bind <+> ptext (sLit "} in"))
      where
        keyword = case bind of
                    Rec _ -> sLit "letrec"
                    NonRec _ _ -> sLit "let"
      -}
    maybe_add_par = if null (cmdLet comm) then noParens else add_par
    cut val frames
      = cat [text "<" <> pprValue val, vcat $ ppr_block "|" ";" ">" $ map ppr_frame frames]
    {-
    cut val []
      = [ptext (sLit "<") <> pprValue val <> ptext (sLit "|>")]
    cut val (frame : cont)
      = ptext (sLit "<") <> pprValue (cmdValue comm) :
        ptext (sLit "|") <+> ppr_frame frame :
        map (\frame -> semi <+> ppr_frame frame) cont ++
        [ptext (sLit ">")]
    -}

ppr_value :: OutputableBndr b => (SDoc -> SDoc) -> Value b -> SDoc
ppr_value _ (Var name) = ppr name
ppr_value add_par (Type ty) = add_par $ ptext (sLit "@") <+> ppr ty
ppr_value add_par (Coercion co) = add_par $ ptext (sLit "CO ...")
ppr_value add_par (Lit lit) = pprLiteral add_par lit
ppr_value add_par value@(Lam _ _)
  = let
      (bndrs, Just body) = collectBinders value
    in
      add_par $
      hang (ptext (sLit "\\") <+> sep (map (pprBndr LambdaBind) bndrs) <+> arrow)
          2 (pprCoreComm body)

collectBinders :: Value b -> ([b], Maybe (Command b))
collectBinders (Lam b comm)
  = go [b] comm
  where
    go bs (Command { cmdLet = [], cmdCont = [], cmdValue = Lam b comm })
      = go (b : bs) comm
    go bs comm
      = (reverse bs, Just comm)
collectBinders val
  = ([], Nothing)

ppr_frame :: OutputableBndr b => Frame b -> SDoc
ppr_frame (App comm)
  = ptext (sLit "[] $") <+> pprParendComm comm
ppr_frame (Case var _ alts)
  = hang (ptext (sLit "case [] of") <+> pprBndr CaseBind var) 2 $
      vcat $ ppr_block "{" ";" "}" (map pprCoreAlt alts)
ppr_frame (Cast co)
  = ptext (sLit "[] `cast` ...")
ppr_frame (Tick ti)
  = ptext (sLit "[] `tick` ...")

pprCoreAlt :: OutputableBndr b => Alt b -> SDoc
pprCoreAlt (Alt con args rhs)
 = hang (ppr_case_pat con args <+> arrow) 2 (pprCoreComm rhs)

ppr_case_pat :: OutputableBndr a => AltCon -> [a] -> SDoc
ppr_case_pat con args
  = ppr con <+> (fsep (map ppr_bndr args))
  where
    ppr_bndr = pprBndr CaseBind

pprParendComm comm = ppr_comm parens comm
pprCoreComm comm = ppr_comm noParens comm
pprParendValue val = ppr_value parens val
pprValue val = ppr_value noParens val

noParens :: SDoc -> SDoc
noParens pp = pp

