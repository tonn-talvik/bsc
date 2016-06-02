{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE ScopedTypeVariables, GADTs #-}
module FallThrough (optFallThrough) where

import Data.Maybe

import Compiler.Hoopl
import IR
import OptSupport

import qualified Data.Map as Map
import Data.Map (Map)

data FallThroughFact = FTF { eq :: Map Label Label, cur :: Maybe Label }

enteringFact :: Label -> FallThroughFact -> FallThroughFact
enteringFact l f = f { eq = x, cur = Nothing }
  where x = case cur f of
              Just l' -> Map.insert l l' eq'
              _ -> eq'
        eq' = (eq f)

unknownFact :: FallThroughFact
unknownFact = FTF { eq = Map.empty, cur = Nothing }

setMiddles :: FallThroughFact -> FallThroughFact
setMiddles f = f { cur = Nothing }

exitingFact :: Label -> FallThroughFact -> FallThroughFact
exitingFact l f = f { cur = Just l }

getFact :: FactBase FallThroughFact -> Label -> FallThroughFact
getFact f l = fromMaybe unknownFact $ lookupFact l f


optFallThrough :: Pass
optFallThrough p@(IProgram { entry = e, body = b}) = 
  do (body', _, _) <- analyzeAndRewriteBwd fallThroughPass (JustC [e]) b mapEmpty
     return $ p { body = body' }


fallThroughPass :: FuelMonad m => BwdPass m Stmnt FallThroughFact
fallThroughPass = BwdPass
  { bp_lattice = fallThroughLattice
  , bp_transfer = fallThroughTransfer
  , bp_rewrite = fallThroughRewrite }


fallThroughLattice :: DataflowLattice FallThroughFact
fallThroughLattice = DataflowLattice
  { fact_name = "remove fall-through jumps"
  , fact_bot  = unknownFact
  , fact_join = join }
  where
    join :: Label -> OldFact FallThroughFact -> NewFact FallThroughFact -> (ChangeFlag, FallThroughFact)
    join _ (OldFact old) (NewFact new) = (ch, j)
      where j = old { eq = (eq old) `Map.union` (eq new) } -- TODO: should we change cur too?
            ch = changeIf (Map.size (eq j) > Map.size (eq old))

fallThroughTransfer :: BwdTransfer Stmnt FallThroughFact
fallThroughTransfer = mkBTransfer bw
  where
    bw :: Stmnt e x -> Fact x FallThroughFact -> FallThroughFact
    bw (SLabel l)         f = enteringFact l f
    bw (STempAssign _ _)  f = setMiddles f
    bw (SLValAssign _ _)  f = setMiddles f
    bw (SProcedure _ _ l) f = setMiddles (getFact f l)
    bw (SExit)            _ = fact_bot fallThroughLattice
    bw (SGoto l)          f = exitingFact l (getFact f l)
    bw (SIfThenElse _ tl fl) f =
      FTF { eq = (eq $ getFact f tl) `Map.union`
                 (eq $ getFact f fl), cur = Nothing }
    bw (SFun _ _ _ l)     f = setMiddles (getFact f l)

fallThroughRewrite :: forall m. FuelMonad m =>
                        BwdRewrite m Stmnt FallThroughFact
fallThroughRewrite = mkBRewrite br
 where
   br :: Stmnt e x -> Fact x FallThroughFact -> m (Maybe (Graph Stmnt e x))
   br (SGoto l) facts = return $
     do l' <- Map.lookup l (eq (getFact facts l))
        returnG $ SGoto l'
   br (SIfThenElse t lt lf) facts = return $
     if ch then returnG $ SIfThenElse t lt' lf' else Nothing
       where 
         look l = fromMaybe l $ Map.lookup l $ eq (getFact facts l)
         lt' = look lt
         lf' = look lf
         ch  = (lt /= lt') || (lf /= lf')
   br (SProcedure p as l) facts = return $
     do l' <- Map.lookup l (eq (getFact facts l))
        returnG $ SProcedure p as l'
   br (SFun t f as l) facts = return $
     do l' <- Map.lookup l (eq (getFact facts l))
        returnG $ SFun t f as l'
   br _ _ = return Nothing

