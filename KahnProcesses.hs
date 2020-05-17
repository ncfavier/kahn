{-# LANGUAGE TypeFamilies #-}
module KahnProcesses where

import Control.Applicative
import Control.Monad
import System.Posix.IO
import System.Posix.Process
import System.IO

import Kahn

type Process = IO

-- `a` est un type fantôme, il n'est là que pour respecter les signatures
newtype Handle1 a = H Handle

instance Kahn IO where
    type InChannel  IO = Handle1
    type OutChannel IO = Handle1

    -- On crée un tube, on transforme les descripteurs en `Handle`, et on
    -- désactive le buffering sur la sortie pour éviter les problèmes.
    newChannel  = do
        (i, o) <- createPipe
        i <- fdToHandle i
        o <- fdToHandle o
        hSetBuffering o NoBuffering
        return (H i, H o)
    -- On écrit la représentation de `a` suivie d'un caractère nul.
    put (H h) a = hPutStr h (show a ++ "\0")
    -- On lit jusqu'au premier caractère nul, et on transforme la chaîne en `a`.
    get (H h)   = fmap read $ many $ mfilter (/= '\0') (hGetChar h)

    -- On utilise l'interface Unix `waitpid` pour attendre les processus.
    doco as = mapM_ waitProcess =<< mapM forkProcess as
        where
        waitProcess pid = do
            r <- getProcessStatus True False pid -- appelle `waitpid`
            case r of
                Just (Exited _) -> return ()
                Just (Terminated _ _) -> return ()
                _ -> waitProcess pid -- si le processus n'a pas fini, on continue d'attendre

    run = id
