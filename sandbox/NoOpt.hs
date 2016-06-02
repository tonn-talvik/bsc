{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE ScopedTypeVariables, GADTs #-}
module NoOpt (optNothing) where

import Compiler.Hoopl
import IR
import OptSupport

data NoOptFact = NA

optNothing :: Pass
optNothing p@(IProgram { entry = e, body = b}) = 
  do (body', _, _) <- analyzeAndRewriteFwd noOptPass (JustC [e]) b (mapSingleton e NA)
     return $ p { body = body' }


noOptPass :: FuelMonad m => FwdPass m Stmnt NoOptFact
noOptPass = FwdPass
  { fp_lattice = noOptLattice
  , fp_transfer = noOptTransfer
  , fp_rewrite = noOptRewrite }


noOptLattice :: DataflowLattice NoOptFact
noOptLattice = DataflowLattice
  { fact_name = "no optimization"
  , fact_bot  = NA
  , fact_join = join }
  where
    join :: Label -> 
              OldFact NoOptFact -> NewFact NoOptFact ->
              (ChangeFlag, NoOptFact)
    join _ (OldFact o) _ = (NoChange, o)

noOptTransfer :: FwdTransfer Stmnt NoOptFact
noOptTransfer = mkFTransfer ft
  where
    ft :: Stmnt e x -> NoOptFact -> Fact x NoOptFact
    ft (SLabel _) f = f
    ft (STempAssign _ _) f = f
    ft (SLValAssign _ _) f = f
    ft (SProcedure _ _ l) f = mapSingleton l f
    ft (SExit) _ = mapEmpty
    ft (SGoto l) f = mapSingleton l f
    ft (SIfThenElse _ tl fl) f = mkFactBase noOptLattice [(tl, f), (fl, f)]
    ft (SFun _ _ _ l) f = mapSingleton l f

noOptRewrite :: forall m. FuelMonad m => FwdRewrite m Stmnt NoOptFact
noOptRewrite = mkFRewrite fr
 where
   fr :: Stmnt e x -> NoOptFact -> m (Maybe (Graph Stmnt e x))
   fr _ _ = return Nothing

