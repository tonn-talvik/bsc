{-# LANGUAGE TypeOperators, GADTs #-}
module IR where

import Data.Maybe (fromJust)

import Language.Pascal.Types hiding (Exit, IfThenElse)

import Compiler.Hoopl
type TempVar = Integer
  
data IProgram = IProgram { 
    entry     :: Label
  , progVars  :: [LVal]
  , tempVars  :: [TempVar]
  , body      :: Graph Stmnt C C }

data Stmnt e x where
  SLabel      :: Label ->                                         Stmnt C O
  STempAssign :: TempVar -> SExpression ->                        Stmnt O O
  SLValAssign :: LVal -> TempVar ->                               Stmnt O O
  --SReturn     :: (Expression :~ a) ->                             Stmnt O C
  SExit       ::                                                  Stmnt O C
  SGoto       :: Label ->                                         Stmnt O C
  SIfThenElse :: TempVar -> Label -> Label        ->              Stmnt O C
  SProcedure  :: Id -> [TempVar] -> Label ->                      Stmnt O C
  SFun        :: TempVar -> Id -> [TempVar] -> Label ->           Stmnt O C

data SExpression =
    SVariable Id
  | SArrayItem Id TempVar
  | SRecordField Id Id
  | SLiteral Lit
  | SOp BinOp TempVar TempVar
  deriving (Eq, Ord)

-- TODO: check record.field usage in analysis for alias-problems!
data LVal =
    LVar Id
  | LArr Id TempVar
  | LFld Id Id
  deriving (Eq, Ord)

instance NonLocal Stmnt where
  entryLabel (SLabel l) = l
  --successors (SReturn _) = []
  successors SExit = []
  successors (SGoto l) = [l]
  successors (SIfThenElse _ t f) = [t, f]
  successors (SProcedure _ _ l) = [l]
  successors (SFun _ _ _ l) = [l]

instance HooplNode Stmnt where
  mkBranchNode = SGoto 
  mkLabelNode = SLabel


instance Ord Lit where
  compare (LInteger l) (LInteger r) = compare l r
  compare (LBool l) (LBool r) = compare l r
  compare (LInteger _) (LBool _) = GT
  compare (LBool _) (LInteger _) = LT
  compare (LString l) (LString r) = compare l r
  compare (LInteger _) (LString _) = GT
  compare (LString _) (LInteger _) = LT
  compare (LBool _) (LString _) = GT
  compare (LString _) (LBool _) = LT

instance Ord BinOp where
  compare l r = compare i j
    where i = fromEnum l
          j = fromEnum r

instance Enum BinOp where
    fromEnum = fromJust . flip lookup table
    toEnum = fromJust . flip lookup (map swap table)
table = [(Add, 0), (Sub, 1), (Mul, 2), (Div, 3), (Mod, 4), (Pow, 5), (IsGT, 6), (IsLT, 7), (IsEQ, 8), (IsNE, 9)]
swap (a, b) = (b, a) -- Data.Tuple ??

