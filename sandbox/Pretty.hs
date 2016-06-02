{-# LANGUAGE GADTs #-}
module Pretty where

import Data.List (intercalate)
import Compiler.Hoopl
import IR

instance Show IProgram where
  show (IProgram { body = b }) = showGraph show b

instance Show LVal where
  show (LVar v) = show v
  show (LArr a t) = show a ++ "[" ++ showTemp t ++ "]"
  show (LFld r f) = show r ++ "." ++ show f

instance Show SExpression where
  show (SVariable v) = show v
  show (SArrayItem a t) = show a ++ "[" ++ showTemp t ++ "]"
  show (SRecordField r f) = show r ++ "." ++ show f
  show (SLiteral l) = show l
  show (SOp o l r) = showTemp l ++ " " ++ show o ++ " " ++ showTemp r

instance Show (Stmnt e x) where
  show (SLabel l) = show l ++ ":\n"
  show (STempAssign t e) = "\t" ++ showTemp t ++ " <- " ++ show e ++ "\n"
  show (SLValAssign lv t) = "\t" ++ show lv ++ " := " ++ showTemp t ++ "\n"
  show (SProcedure p as l) = "\t" ++ p ++ " " ++ showTemps as ++ " goto " ++ show l
  show (SFun lv f as l) = "\t" ++ showTemp lv ++ " <- " ++ f ++ showTemps as ++ " goto " ++ show l
  --show (SReturn r) = "return " ++ show r
  show (SIfThenElse e t f) = "\tif " ++ showTemp e ++ " then " ++ show t ++ " else " ++ show f ++ "\n"
  show (SGoto l) = "\tgoto " ++ show l
  show SExit = "\texit"

showTemp :: TempVar -> String
showTemp t = "t" ++ show t

showTemps :: [TempVar] -> String
showTemps ts = "(" ++ intercalate ", " (map showTemp ts) ++ ")"

