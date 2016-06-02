{-# LANGUAGE GeneralizedNewtypeDeriving #-} 

module MonadUnique
        ( UniqueT,
          Unique,
          MonadUnique,
          fresh,
          evalUniqueT,
          evalUnique, mapUniqueT,
          used )
    where
 
import Control.Monad
import Control.Monad.State
import Control.Monad.Identity
 
newtype UniqueT m a = UniqueT (StateT Integer m a)
    deriving (Functor, Monad, MonadTrans, MonadIO)
 
newtype Unique a = Unique (UniqueT Identity a)
    deriving (Functor, Monad, MonadUnique)
 
class Monad m => MonadUnique m where
    fresh :: m Integer
    used :: m [Integer]
 
instance (Monad m) => MonadUnique (UniqueT m) where
    fresh = UniqueT $ do
                n <- get
                put (succ n)
                return n
    used = UniqueT $ do
                   n <- get
                   return [0..n]
 
evalUniqueT (UniqueT s) = evalStateT s 0
evalUnique (Unique s) = runIdentity (evalUniqueT s)


{-
mapUniqueT :: (Monad m, Functor m) => (m a -> m b) -> UniqueT m a -> UniqueT m b
mapUniqueT f (UniqueT m) = UniqueT $ mapStateT
  (\m -> do (a, s) <- m
            b <- f (fmap fst m)
            return (b, s)) m
-}
 

mapUniqueT :: (Monad m) => (m a -> m b) -> UniqueT m a -> UniqueT m b
mapUniqueT f (UniqueT m) = UniqueT $ mapStateT 
  (\m -> do (_, s) <- m
            b <- f $ m >>= return . fst
            return (b, s)) m

