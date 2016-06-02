{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE ScopedTypeVariables, GADTs #-}
module CSE (optCSE) where
-- common subexpression elimination (CSE) 

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe)

import Compiler.Hoopl
import IR
import OptSupport

-- do we have bug, when using function calls in expressions, i.e. we eliminate side-effects too!
--       - NO. we don't consider function calls, only assignments
-- TODO: some operations are commutative - could be used when comparing equality of expressions
-- TODO: rethink data structures

optCSE :: Pass
optCSE p@(IProgram { entry = e, body = b}) = 
  do (body', _, _) <- analyzeAndRewriteFwd availExprPass (JustC [e]) b $ mapSingleton e (fact_bot availLattice)
     return $ p { body = body' }

availExprPass :: FuelMonad m => FwdPass m Stmnt AvailExprFact
availExprPass = FwdPass
  { fp_lattice  = availLattice
  , fp_transfer = availTransfer
  , fp_rewrite  = commonRewrite }


data AvailExprFact = AvailExprFact 
  { refs       :: Map TempVar SExpression
  , firstUsage :: Map SExpression (Maybe TempVar)
  } deriving Eq

lookupFirst :: AvailExprFact -> TempVar -> Maybe TempVar
lookupFirst f t = 
  do w <- Map.lookup t (refs f)
     v <- Map.lookup w (firstUsage f)
     v

lookupTemp :: AvailExprFact -> TempVar -> TempVar
lookupTemp f t = fromMaybe t $ lookupFirst f t

lookupTemps :: AvailExprFact -> [TempVar] -> [TempVar]
lookupTemps f ts = map (lookupTemp f) ts

availLattice :: DataflowLattice AvailExprFact
availLattice = DataflowLattice
  { fact_name = "Variable X is reused - remove duplicate TempAssign"
  , fact_bot  = AvailExprFact { refs = Map.empty, firstUsage = Map.empty }
  , fact_join = join }
  where
    join _ (OldFact old) (NewFact new)
      = let combine = \u v -> if u == v then v else Nothing
            ref = Map.union (refs new) (refs old)
            first = Map.unionWith combine
                      (firstUsage new) (firstUsage old)
            f = AvailExprFact { refs = ref, firstUsage = first }
            ch = changeIf (old /= f)
        in (ch, f)

availTransfer :: FwdTransfer Stmnt AvailExprFact
availTransfer = mkFTransfer ft
 where
  ft :: Stmnt e x -> AvailExprFact -> Fact x AvailExprFact
  ft (STempAssign _ (SArrayItem _ _)) f = f -- avoid aliasing
  ft (STempAssign t e)     f = f { refs = Map.insert t e (refs f)
                                 , firstUsage = Map.insertWith 
                                                  (\n o -> if o == Nothing then n else o) 
                                                  e (Just t) (firstUsage f) }

  ft (SLValAssign lv _)    f =
     f { firstUsage = Map.insert (fromLVal lv) 
                        Nothing (firstUsage f) } -- kill

  ft (SLabel _)            f = f
  ft (SExit)               _ = mapEmpty
  ft (SGoto l      )       f = mapSingleton l f
  ft (SIfThenElse _ tl fl) f = mkFactBase availLattice [(tl, f), (fl, f)]
  ft (SProcedure _ _ l)    f = mapSingleton l f
  -- this is overkill, considering we have already generated ~unique~ temp-vars
  --ft (SFun t _ _ l)        f = mapSingleton l (f { refs = Map.delete t (refs f) } )
  ft (SFun _ _ _ l)        f = mapSingleton l f


-- TODO: check array ALIASING, see expr4.pas for ideas, how current implementation is wrong!
-- TODO: commonRewrite and copyRewrite are too similar; maybe map-over-temps would be better?
commonRewrite :: forall m. FuelMonad m => FwdRewrite m Stmnt AvailExprFact
commonRewrite = mkFRewrite rw
  where
    rw :: Stmnt e x -> AvailExprFact -> m (Maybe (Graph Stmnt e x))
    rw (SLValAssign (LArr a i) t) f
      | i /= i' = return $
        returnG $ SLValAssign (LArr a i') t
      where i' = lookupTemp f i
    rw (SLValAssign lv t) f = return $
      do t' <- lookupFirst f t
         returnG $ SLValAssign lv t'
    -- how to combine L and R computations?
    rw (STempAssign t (SOp o l r)) f = return $ returnG $
      STempAssign t $ SOp o l' r'
        where l' = lookupTemp f l
              r' = lookupTemp f r
    rw (STempAssign t (SArrayItem a i)) f = return $
      do i' <- lookupFirst f i
         returnG $ STempAssign t (SArrayItem a i')
    rw (SIfThenElse t tl fl) f = return $
      do t' <- lookupFirst f t
         returnG $ SIfThenElse t' tl fl
    rw (SProcedure p ts l) f = return $ returnG $
      let ts' = lookupTemps f ts
      in SProcedure p ts' l
    rw (SFun t p ts l) f = return $ returnG $
      let ts' = lookupTemps f ts
      in SFun t p ts' l
    rw _ _ = return Nothing


