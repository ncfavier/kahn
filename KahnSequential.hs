{-# LANGUAGE MultiParamTypeClasses #-}
module KahnSequential where

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef

import Kahn

data Queue a = Queue [a] [a]

emptyQueue = Queue [] []

push x (Queue i o) = Queue (x:i) o

pop (Queue i (x:o)) = Just (x, Queue i o)
pop (Queue [] [])   = Nothing
pop (Queue i [])    = pop (Queue [] (reverse i))

newtype IOQueue a = Q (IORef (Queue a))

newIOQueue = Q <$> newIORef emptyQueue

pushIOQueue (Q q) x = modifyIORef q (push x)

takeIOQueue (Q q) = do
    m <- pop <$> readIORef q
    case m of
        Nothing -> return Nothing
        Just (x, q') -> Just x <$ writeIORef q q'

data Process a = Pure a | Atom (IO (Process a))

instance Functor Process where
    fmap = liftM

instance Applicative Process where
    pure = Pure
    (<*>) = ap

instance Monad Process where
    return = Pure
    Pure x >>= f = f x
    Atom m >>= f = Atom ((>>= f) <$> m)

instance MonadIO Process where
    liftIO m = Atom (Pure <$> m)

instance Kahn Process IOQueue IOQueue where
    newChannel = (\q -> (q, q)) <$> liftIO newIOQueue
    put q a    = liftIO (pushIOQueue q a)
    get q      = maybe (get q) return =<< liftIO (takeIOQueue q)

    doco [] = return ()
    doco as = doco =<< sequence [liftIO m | Atom m <- as]

    run (Pure x) = return x
    run (Atom m) = run =<< m
