{-# LANGUAGE MultiParamTypeClasses #-}
module KahnSockets where

import Control.Applicative
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import System.IO
import Network.Socket

import Kahn

type Process = IO

newtype Handle1 a = H Handle

instance Kahn IO Handle1 Handle1 where
    newChannel = do
        sock <- socket AF_INET Stream defaultProtocol
        setSocketOption sock ReuseAddr 1
        bind sock (SockAddrInet defaultPort localhost)
        port <- socketPort sock
        listen sock 1024
        v <- newEmptyMVar
        forkIO $ putMVar v . fst =<< accept sock
        client <- socket AF_INET Stream defaultProtocol
        connect client (SockAddrInet port localhost)
        server <- takeMVar v
        i <- socketToHandle server ReadMode
        o <- socketToHandle client WriteMode
        return (H i, H o)
        where localhost = tupleToHostAddress (127, 0, 0, 1)

    put (H h) a = hPutStr h (show a ++ "\0")
    get (H h)   = fmap read $ many $ mfilter (/= '\0') (hGetChar h)

    doco as = mapM_ takeMVar =<< mapM fork as
        where
        fork a = do
            v <- newEmptyMVar
            forkFinally a (\_ -> putMVar v ())
            return v

    run = id
