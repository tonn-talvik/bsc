{-# LANGUAGE TypeOperators, ViewPatterns #-}
module Ast2Ir where

import Language.Pascal.Types
import IR
import Compiler.Hoopl
import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Data.Map (Map)
import MonadUnique
import Control.Monad.Trans
import Control.Monad.Reader


type TranslationMonad = UniqueT (ReaderT (Map String Label) SimpleUniqueMonad)
runTranslationMonad = runSimpleUniqueMonad . (flip runReaderT Map.empty) . evalUniqueT

runAstToIr :: Program :~ TypeAnn -> IProgram
runAstToIr = runTranslationMonad . astToIr

freshT :: TranslationMonad TempVar
freshT = fresh

freshL :: TranslationMonad Label
freshL = lift $ lift freshLabel

usedTs :: TranslationMonad [TempVar]
usedTs = used

getL :: String -> TranslationMonad Label
getL k = lift $ asks (fromJust . Map.lookup k)

localL :: [(String, Label)] -> TranslationMonad a -> TranslationMonad a
localL ls = mapUniqueT (local (\e -> foldl (\r (s, l) -> Map.insert s l r) e ls))

astVarToIrVars :: Annotate Symbol TypeAnn -> [LVal]
astVarToIrVars v =
  let s = content v
      t = annotation v 
  in case typeOf t of
      TRecord fields -> do f <- fields
                           return $ LFld (symbolName s) (fst f)
      _ -> return $ LVar (symbolName s) -- not quite correct, e.g. arrays?!

astToIr :: Program :~ TypeAnn -> TranslationMonad IProgram
astToIr (content -> Program {progVariables = vars, progBody = body }) =
  do lentry <- freshL
     gs <- ss2g body
     temps <- usedTs
     return $ IProgram 
       { entry = lentry
       , progVars = concat $ map astVarToIrVars vars
       , tempVars = temps
       , body = mkLabel lentry <*> gs <*> mkLast SExit }


ss2g :: [Statement :~ a] -> TranslationMonad (Graph Stmnt O O)
ss2g ss =
   do gs <- mapM s2g ss
      return $ catGraphs gs


s2g :: Statement :~ a -> TranslationMonad (Graph Stmnt O O)

s2g forStatement @ (content -> For i e0 eh body) =
  do -- labels
     ltest <- freshL
     lbody <- freshL
     linc <- freshL
     lafter <- freshL
     -- initializiation part
     let a = flip Annotate $ annotation forStatement -- abusing for-statement annotation
         h = i ++ "~High"
         lvari = a $ LVariable i
         lvarh = a $ LVariable h
         s0 = a $ Assign lvari e0
         sh = a $ Assign lvarh eh
     g0 <- s2g s0
     gh <- s2g sh
     -- test part
     let vari = a $ Variable i
         varh = a $ Variable h
         test = a $ Op IsGT vari varh
     (t, gtest) <- e2g test
     let sif = SIfThenElse t lafter lbody
     -- increment part
     let sinc = a $ Assign lvari $ a $ Op Add vari $ a $ Literal $ LInteger 1
     ginc <- s2g sinc
     -- body part
     gbody <- localL [("continue", linc), ("break", lafter)] (ss2g body)
     -- loop
     return $ 
       g0 <*> gh <*> mkBranch ltest |*><*| 
       mkLabel ltest <*> gtest <*> mkLast sif |*><*|
       mkLabel lbody <*> gbody <*> mkBranch linc |*><*|
       mkLabel linc <*> ginc <*> mkBranch ltest |*><*|
       mkLabel lafter

s2g (content -> IfThenElse e ts fs) =
  do endif <- freshL
     ltrue <- freshL
     lfalse <- freshL
     (t, g) <- e2g e
     tbranch <- ss2g ts
     fbranch <- ss2g fs
     return $
       g <*> mkLast (SIfThenElse t ltrue lfalse) |*><*|
       mkLabel ltrue  <*> tbranch <*> mkBranch endif |*><*|
       mkLabel lfalse <*> fbranch <*> mkBranch endif |*><*|
       mkLabel endif

s2g (content -> Assign lval exp) = 
  do (t, g) <- e2g exp
     (lv, lg) <- case content lval of
                      LVariable v -> return (LVar v, emptyGraph)
                      LField r f -> return (LFld r f, emptyGraph)
                      LArray a e -> do (t, g) <- e2g e
                                       return (LArr a t, g)
     let ag = mkMiddle $ SLValAssign lv t
     return $ g <*> lg <*> ag

s2g (content -> Procedure p es) = 
  do (ts, es') <- mapAndUnzipM e2g es
     l <- freshL
     let es'' = catGraphs es'
         g = mkLast $ SProcedure p ts l
         r = mkLabel l -- where to return after call
     return $ es'' <*> g |*><*| r

-- TODO: Continue, break and exit consume surrogate entry-labels... although they get thrown away after opt?
s2g (content -> Continue) = 
  do lsur <- freshL
     lcon <- getL "continue"
     return $
       mkBranch lcon |*><*|
       mkLabel lsur
s2g (content -> Break) = 
  do lsur <- freshL
     lbrk <- getL "break"
     return $
       mkBranch lbrk |*><*|
       mkLabel lsur
s2g (content -> Exit) = 
  do lsur <- freshL
     return $
       mkLast SExit |*><*|
       mkLabel lsur

--s2g _ = return emptyGraph


e2g :: (Expression :~ a) -> TranslationMonad (TempVar, Graph Stmnt O O)
e2g (content -> Variable v) = 
  do t <- freshT
     let g = mkMiddle $ STempAssign t (SVariable v)
     return (t, g)
e2g (content -> Literal l) = 
  do t <- freshT
     let g = mkMiddle $ STempAssign t (SLiteral l)
     return (t, g)
e2g (content -> Op o el er) = 
  do (tl, gl) <- e2g el
     (tr, gr) <- e2g er
     t <- freshT
     let g = mkMiddle $ STempAssign t $ SOp o tl tr
     return (t, gl <*> gr <*> g)
e2g (content -> Call f es) = 
  do (ts, es') <- mapAndUnzipM e2g es
     l <- freshL
     t <- freshT
     let es'' = catGraphs es'
         g = mkLast $ SFun t f ts l 
         r = mkLabel l -- where to return after call
     return (t, es'' <*> g |*><*| r)
e2g (content -> ArrayItem a e) = 
  do (u, e') <- e2g e
     t <- freshT
     let g = mkMiddle $ STempAssign t $ SArrayItem a u
     return (t, e' <*> g)
e2g (content -> RecordField r f) = 
  do t <- freshT
     let g = mkMiddle $ STempAssign t $ SRecordField r f
     return (t, g)

