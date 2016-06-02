import System.Environment


import Language.Pascal.Parser
import Language.Pascal.TypeCheck
import Language.Pascal.CodeGen
import Language.SSVM.Types
import Language.SSVM.Interpreter


main :: IO ()
main = do
     args <- getArgs 
     parsed_source <- parseSource (head args)
     print parsed_source

     let r = checkTypes parsed_source 
     let g = runCodeGen $ generate r
         code = g { cCode = reverse (cCode g) }
     print code

     let h = interpret code
     putStrLn "running"
     runVM h
