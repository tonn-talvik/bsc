import Control.Monad.Reader
import Control.Monad.State hiding (fmap)
import Data.Map
import Prelude hiding (lookup, fmap)
import MonadUnique
import Data.Maybe

type Params = Map String Int
type St = StateT String 
type Tt = UniqueT

defaultParams :: Params
defaultParams = fromList [("a", 64), ("abc", 1), ("b", 0)]

greet :: Params -> String
greet ps = let x = lookup "b" ps
           in "hello " ++ show x

look :: (Monad m) => String -> ReaderT Params m Int
look k = asks (fromJust . lookup k)

slook ::(Monad m) => String -> ReaderT Params m String
slook k = look k >>= return . show

bye :: String -> Tt (ReaderT Params (State String)) String
bye who = do t <- fresh
             x <- lift $ look "abc"
             let s = show x
             return $ "t=" ++ show t ++ "abc=" ++ s

greets :: Tt (ReaderT Params (State String)) String
greets = do t0 <- fresh
            s <- mapUniqueT (local (insert "abc" 123)) (bye "myself ")
            abc <- lift $ slook "abc"
            t1 <- fresh
            return $ "t0=" ++ show t0 ++ s  ++ " abc was " ++ abc ++ " t1=" ++ show t1

main = do
         print $ greet defaultParams
         print $ runState (runReaderT (evalUniqueT greets) defaultParams) ""
