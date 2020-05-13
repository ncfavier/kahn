{-# LANGUAGE MultiParamTypeClasses #-}
module KahnProcesses where

import Control.Applicative
import Control.Monad
import System.Posix.IO
import System.Posix.Process
import System.IO

import Kahn

type Process = IO

newtype Handle1 a = H Handle

instance Kahn IO Handle1 Handle1 where
    newChannel  = do
        (i, o) <- createPipe
        i <- fdToHandle i
        o <- fdToHandle o
        hSetBuffering o NoBuffering
        return (H i, H o)
    put (H h) a = hPutStr h (show a ++ "\0")
    get (H h)   = fmap read $ many $ mfilter (/= '\0') (hGetChar h)

    doco as = mapM_ waitProcess =<< mapM forkProcess as
        where
        waitProcess pid = do
            r <- getProcessStatus True False pid
            case r of
                Just (Exited _) -> return ()
                Just (Terminated _ _) -> return ()
                _ -> waitProcess pid

    run = id
