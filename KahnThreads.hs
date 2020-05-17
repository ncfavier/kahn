{-# LANGUAGE TypeFamilies #-}
module KahnThreads where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan

import Kahn

type Process = IO

instance Kahn IO where
    type InChannel  IO = Chan
    type OutChannel IO = Chan

    -- On utilise les fonctions fournies par Control.Concurrent.Chan
    newChannel = (\c -> (c, c)) <$> newChan
    put c a    = writeChan c a >> yield
    get        = readChan

    -- La partie importante est `mapM fork as`, le reste est un "hack" pour
    -- attendre la fin d'un thread : on crée une MVar vide, dans laquelle le
    -- thread écrit pour signaler qu'il a fini de s'exécuter. On utilise `takeMVar`
    -- pour bloquer jusqu'à ce que la MVar soit pleine.
    doco as = mapM_ takeMVar =<< mapM fork as
        where
        fork a = do
            v <- newEmptyMVar
            forkFinally a (\_ -> putMVar v ())
            return v

    run = id
