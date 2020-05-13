{-# LANGUAGE MultiParamTypeClasses #-}
module KahnThreads where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan

import Kahn

type Process = IO

instance Kahn IO Chan Chan where
    newChannel = (\c -> (c, c)) <$> newChan
    put c a    = writeChan c a >> yield
    get        = readChan

    doco as = mapM_ takeMVar =<< mapM fork as
        where
        fork a = do
            v <- newEmptyMVar
            forkFinally a (\_ -> putMVar v ())
            return v

    run = id
