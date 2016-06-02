{-# OPTIONS_GHC -Wall -fno-warn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables, GADTs #-}
module Live (optDead) where

import Data.Maybe
import qualified Data.Set as Set
import Data.Set (Set)

import Compiler.Hoopl
import IR
import OptSupport

type Live = Set Var

optDead :: Pass
optDead p@(IProgram { entry = e, body = b}) = 
  do (body', _, _) <- analyzeAndRewriteBwd deadElimPass (JustC [e]) b mapEmpty
     return $ p { body = body' }

deadElimPass :: FuelMonad m => BwdPass m Stmnt Live
deadElimPass = BwdPass 
 { bp_lattice = liveLattice
 , bp_transfer = liveness
 , bp_rewrite = deadAsstElim }

liveLattice :: DataflowLattice Live
liveLattice = DataflowLattice
  { fact_name = "Live variables"
  , fact_bot  = Set.empty
  , fact_join = add
  }
    where add _ (OldFact old) (NewFact new) = (ch, j)
            where
              j = new `Set.union` old
              ch = changeIf (Set.size j > Set.size old)

liveness :: BwdTransfer Stmnt Live
liveness = mkBTransfer live
  where
    live :: Stmnt e x -> Fact x Live -> Live
    live (SLabel _)             f = f
    live (STempAssign t e)      f =
      case e of
        SVariable _ -> Set.insert (toPVar e) f'
        SArrayItem a i -> Set.insert (PVar (LVar a)) $
                          Set.insert (TVar i) f'
        SRecordField _ _ -> Set.insert (toPVar e) f'
        SOp _ l r -> Set.insert (TVar l) $ Set.insert (TVar r) f'
        _ -> f'
      where f' = Set.delete (TVar t) f
    live (SLValAssign lv t)     f =
      case lv of
        LArr _ i -> Set.insert (TVar i) f'
        _ -> Set.delete (toPVar $ fromLVal lv) f'
      where f' = Set.insert (TVar t) f
    live (SGoto l)              f = fact f l
    live (SIfThenElse t tl fl)  f = Set.insert (TVar t) (fact f tl `Set.union` fact f fl) 
    live (SProcedure _ ts l)    f = fact f l `Set.union` Set.fromList (map TVar ts)
    live (SFun t _ ts l)        f = (Set.delete (TVar t) (fact f l)) `Set.union`
                                    Set.fromList (map TVar ts)
    live SExit                  _ = Set.empty

    fact :: FactBase Live -> Label -> Live
    fact f l = fromMaybe Set.empty $ lookupFact l f

deadAsstElim :: forall m . FuelMonad m => BwdRewrite m Stmnt Live
deadAsstElim = mkBRewrite d
  where
    d :: Stmnt e x -> Fact x Live -> m (Maybe (Graph Stmnt e x))
    d (STempAssign t _) live
        | not ((TVar t) `Set.member` live) = return $ Just emptyGraph
    d (SLValAssign (LArr a _) _) live  = return $ 
      if (PVar (LVar a)) `Set.member` live
      then Nothing
      else Just emptyGraph
    d (SLValAssign lv _) live
        | not ((PVar lv) `Set.member` live) = return $ Just emptyGraph
    d _ _ = return Nothing
                                                     
