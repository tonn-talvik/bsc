{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE ScopedTypeVariables, GADTs #-}
module CopyProp (optCopy) where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe

import Compiler.Hoopl
import IR
import OptSupport

optCopy :: Pass
optCopy p@(IProgram { entry = e, progVars = vs, tempVars = ts, body = b}) = 
  do (body', _, _) <- analyzeAndRewriteFwd copyPropPass (JustC [e]) b $ mapSingleton e (initFact (vs, ts))
     return $ p { body = body' }

initFact :: ([LVal], [TempVar]) -> CopyFact
initFact (vs, ts) = Map.fromList $
    [(TVar t, Top) | t <- ts] ++ 
    [(PVar v, Top) | v <- vs] 


copyPropPass :: FuelMonad m => FwdPass m Stmnt CopyFact
copyPropPass = FwdPass
  { fp_lattice = copyLattice
  , fp_transfer = copyTransfer
  , fp_rewrite = copyRewrite }


--data CopyFact = CopyFact 
  --{ src :: Map.Map TempVar (WithTop Id)
  --, dst :: Map.Map Id (WithTop TempVar) } deriving Eq

type CopyFact = Map Var (WithTop Var)

lookupCopy :: CopyFact -> Var -> Maybe Var
lookupCopy f v = do PElem v' <- Map.lookup v f
                    PElem v'' <- Map.lookup v' f
                    return v''

lookupTemp :: CopyFact -> TempVar -> TempVar
lookupTemp f t = fromTVar $ fromMaybe (TVar t) $ lookupCopy f (TVar t)

lookupTemps :: CopyFact -> [TempVar] -> [TempVar]
lookupTemps f = map $ lookupTemp f

copy :: Var -> Var -> CopyFact -> CopyFact
copy src dst f = Map.insert dst (PElem src) f

kill :: Var -> CopyFact -> CopyFact
kill dst f = Map.insert dst Top f

copyLattice :: DataflowLattice CopyFact
copyLattice = DataflowLattice
  { fact_name = "Copy dst <- temp <- src"
  , fact_bot  = Map.empty
  , fact_join = joinMaps (extendJoinDomain join) }
  where
    join _ (OldFact old) (NewFact new)
      = if new == old then (NoChange, PElem new)
        else (SomeChange, Top)

copyTransfer :: FwdTransfer Stmnt CopyFact
copyTransfer = mkFTransfer ft
  where
    ft :: Stmnt e x -> CopyFact -> Fact x CopyFact
    ft (STempAssign t e)          f 
      | isExprVar e                 = copy (toPVar e) (TVar t) f
      | otherwise                   = kill (TVar t) f
    ft (SLValAssign (LArr _ _) _) f = f -- avoid aliasing problems!
    ft (SLValAssign lv t)         f = copy (TVar t) (PVar lv) f
    ft (SLabel _)                 f = f
    ft (SExit)                    _ = mapEmpty
    ft (SGoto l      )            f = mapSingleton l f
    ft (SIfThenElse _ tl fl)      f = mkFactBase copyLattice [(tl, f), (fl, f)]
    ft (SProcedure _ _ l)         f = mapSingleton l f
    ft (SFun _ _ _ l)             f = mapSingleton l f -- overkill: $ kill (TVar t) f



-- TODO: why do we stop, when rewriting proc/fun?
copyRewrite :: forall m. FuelMonad m => FwdRewrite m Stmnt CopyFact
copyRewrite = mkFRewrite rw
  where
    rw :: Stmnt e x -> CopyFact -> m (Maybe (Graph Stmnt e x))
    rw (STempAssign t e) f | isExprVar e = return $
      do (PVar lv) <- lookupCopy f (toPVar e)
         returnG $ STempAssign t (fromLVal lv)
    rw (STempAssign t (SOp o l r)) f = return $
      let l' = lookupTemp f l
          r' = lookupTemp f r
      in returnG $ STempAssign t (SOp o l' r')
    rw (SLValAssign (LArr a i) t) f
      | i /= i' = return $
          returnG $ SLValAssign (LArr a i') t
        where i' = lookupTemp f i
    rw (SLValAssign lv t) f = return $
      do (TVar t') <- lookupCopy f (TVar t)
         returnG $ SLValAssign lv t'
    rw (SIfThenElse t tl fl) f = return $
      do (TVar t') <- lookupCopy f (TVar t)
         returnG $ SIfThenElse t' tl fl
    rw (SProcedure p ts l) f = return $
      let ts' = lookupTemps f ts
      in returnG $ SProcedure p ts' l
    rw (SFun t p ts l) f = return $
      let ts' = lookupTemps f ts
      in returnG $ SFun t p ts' l
    rw _ _ = return Nothing

