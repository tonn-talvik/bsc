{-# LANGUAGE GADTs #-}
module OptSupport where

import Compiler.Hoopl
--import Ast2Ir (L)
import IR

stmntToG :: Stmnt e x -> Graph Stmnt e x
stmntToG s@(SLabel _) = mkFirst s
stmntToG s@(STempAssign _ _) = mkMiddle s
stmntToG s@(SLValAssign _ _) = mkMiddle s
stmntToG s@(SProcedure _ _ _) = mkLast s
stmntToG s@(SExit) = mkLast s
stmntToG s@(SGoto _) = mkLast s
stmntToG s@(SIfThenElse _ _ _) = mkLast s
stmntToG s@(SFun _ _ _ _) = mkLast s

returnG :: Stmnt e x -> Maybe (Graph Stmnt e x)
returnG = return . stmntToG

type Pass = IProgram -> InfiniteFuelMonad SimpleUniqueMonad IProgram
--type Pass = Program -> CheckingFuelMonad L Program

optimize :: Pass -> IProgram -> IProgram
optimize o p = runSimpleUniqueMonad $ runWithFuel 1 $ o p

data Var = TVar TempVar | PVar LVal
  deriving (Eq, Ord)

isExprVar :: SExpression -> Bool
isExprVar (SVariable _) = True
isExprVar (SArrayItem _ _) = True
isExprVar (SRecordField _ _) = True
isExprVar _ = False

toPVar :: SExpression -> Var
toPVar (SVariable v) = PVar $ LVar v
toPVar (SArrayItem a i) = PVar $ LArr a i
toPVar (SRecordField r f) = PVar $ LFld r f
toPVar _ = error "unable to convert SExpression to Var"

fromLVal :: LVal -> SExpression
fromLVal (LVar v) = SVariable v
fromLVal (LArr a i) = SArrayItem a i
fromLVal (LFld r f) = SRecordField r f

fromPVar :: Var -> SExpression
fromPVar (PVar lv) = fromLVal lv
fromPVar _ = error "unable to convert from Var to SExpression"

fromTVar :: Var -> TempVar
fromTVar (TVar t) = t
fromTVar _ = error "unable to convert from Var to TempVar"
