{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE ScopedTypeVariables, GADTs #-}
module ConstProp (optConst) where

import qualified Data.Map as Map
import Data.Map (Map)

import Data.Maybe (fromJust)
import Compiler.Hoopl
import IR
import OptSupport
import Language.Pascal.Types

optConst :: Pass
optConst p@(IProgram { entry = e
                     , progVars = vs
                     , tempVars = ts
                     , body = b}) = 
  do (body', _, _) <- analyzeAndRewriteFwd 
                        constPropPass (JustC [e]) b $ 
                        mapSingleton e (initFact (vs, ts))
     return $ p { body = body' }

initFact :: ([LVal], [TempVar]) -> ConstFact
initFact (vs, ts) = Map.fromList $
    [(TVar t, Top) | t <- ts] ++
    [(PVar v, Top) | v <- vs]
  

constPropPass :: FuelMonad m => FwdPass m Stmnt ConstFact
constPropPass = FwdPass
  { fp_lattice  = constLattice
  , fp_transfer = varHasLit
  , fp_rewrite  = simplify }

type ConstFact = Map Var (WithTop Lit)

constLattice :: DataflowLattice ConstFact
constLattice = DataflowLattice
  { fact_name = "temp <- literal"
  , fact_bot  = Map.empty
  , fact_join = joinMaps (extendJoinDomain constFactAdd) }
  where
    constFactAdd _ (OldFact old) (NewFact new) 
      = if new == old then (NoChange, PElem new)
        else               (SomeChange, Top)


varHasLit :: FwdTransfer Stmnt ConstFact
varHasLit = mkFTransfer ft
  where
    ft :: Stmnt e x -> ConstFact -> Fact x ConstFact
    ft (STempAssign t e)     f =
      Map.insert (TVar t) (case e of 
                             SLiteral x -> PElem x
                             _ -> Top) f
  
    ft (SLValAssign lv t)    f = Map.insert (PVar lv)
                                   (fromJust $ Map.lookup (TVar t) f) f

    ft (SLabel _)            f = f
    ft (SExit)               _ = mapEmpty
    ft (SGoto l      )       f = mapSingleton l f
    ft (SIfThenElse t tl fl) f =
      mkFactBase constLattice 
        [ (tl, Map.insert (TVar t) (PElem (LBool True)) f)
        , (fl, Map.insert (TVar t) (PElem (LBool False)) f) ]
    ft (SProcedure _ _ l)    f = mapSingleton l f
    ft (SFun _ _ _ l)        f = mapSingleton l f


simplify :: forall m. FuelMonad m => FwdRewrite m Stmnt ConstFact
simplify = mkFRewrite fold
  where
    fold :: Stmnt e x -> ConstFact -> m (Maybe (Graph Stmnt e x))
    -- fold if-then-else into goto
    fold (SIfThenElse t tl fl) f = return $
      do (LBool t') <- lookup (TVar t) f
         returnG $ SGoto $
           if t' then tl else fl 	
    fold (STempAssign t (SOp o l r)) f = return $
      do l' <- lookup (TVar l) f
         r' <- lookup (TVar r) f
         returnG $ STempAssign t $ SLiteral (eval (o, l', r'))
    fold (STempAssign t e) f
      | isExprVar e = return $
        do x <- lookup (toPVar e) f
           returnG $ STempAssign t $ SLiteral x
    fold _ _ = return Nothing

    lookup :: Var -> ConstFact -> Maybe Lit
    lookup t f = 
      case Map.lookup t f of
        Just (PElem v) -> Just v
        _ -> Nothing

    eval :: (BinOp, Lit, Lit) -> Lit
    eval (Add, LInteger x, LInteger y) = LInteger (x + y)
    eval (Sub, LInteger x, LInteger y) = LInteger (x - y)
    eval (Mul, LInteger x, LInteger y) = LInteger (x * y)
    eval (Div, LInteger x, LInteger y) = LInteger (x `div` y)
    eval (Mod, LInteger x, LInteger y) = LInteger (x `mod` y)
    eval (Pow, LInteger x, LInteger y) = LInteger (x ^ y)
    eval (IsLT, x, y) = LBool (x < y)
    eval (IsGT, x, y) = LBool (x > y)
    eval (IsEQ, x, y) = LBool (x == y)
    eval (IsNE, x, y) = LBool (x /= y)
    eval _ = error "TODO: more evaluations, if possible"

