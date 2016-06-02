{-# LANGUAGE RankNTypes, TypeOperators, ViewPatterns #-}
import System.Environment

import Prelude hiding (mapM_)
import Language.Pascal.Parser
import Language.Pascal.TypeCheck
import Language.Pascal.CodeGen
import Language.SSVM.Types
import Language.SSVM.Interpreter

import Compiler.Hoopl
import Data.Foldable

import MonadUnique
import Ast2Ir
import IR

import NoOpt
import ConstProp
import CopyProp
import CSE
import Live
import FallThrough

import Pretty
import OptSupport

main :: IO ()
main = do
     args <- getArgs 
     mapM_ optFile args

optFile :: String -> IO ()
optFile f = do
     parsedSource <- parseSource f
     let typeCheckedSource = checkTypes parsedSource 
     header "type-checked source"
     print typeCheckedSource
     --let generatedCode = runCodeGen $ generate typeCheckedSource
     --header "generated unoptimized code"
     --print generatedCode

     let ir = runAstToIr typeCheckedSource
         os = [ (return, "intermediate representation")
              , (optConst, "const-folding-propagation")
              , (optCSE, "common-subexpressions-elimination")
              , (optCopy, "copy-propagation")
              , (optDead, "eliminate-dead-assignments")
              , (optFallThrough, "optimize fallthrough labels")
              , (optNothing, "no optimization - should get rid of any extra blocks in the graph")
              ]
     foldlM (\ir (pass, comment) ->
       do header comment
          let o = optimize pass ir
          print o
          return o) ir os

     putStrLn "done"

     where header s = putStrLn $ "------" ++ s ++ "------"


