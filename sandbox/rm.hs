import Control.Monad.Reader
import Data.Map
import Prelude hiding (lookup)

type Params = Map String Int

defaultParams :: Params
defaultParams = fromList [("a", 64), ("abc", 1), ("b", 0)]

greet :: Params -> String
greet ps = let x = lookup "b" ps
           in "hello " ++ show x

bye :: Reader Params String
bye = do abc <- asks (show . lookup "abc")
         return $ " abc is " ++ abc

greets :: Reader Params String
greets = do b <- asks (show . lookup "b")
            s <- local (insert "abc" 123) bye
            abc <- asks (show . lookup "abc")
            return $ "hello " ++ b ++ s ++ " abc was " ++ abc

main = do
         print $ greet defaultParams
         print $ runReader greets defaultParams
