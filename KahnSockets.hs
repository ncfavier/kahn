{-# LANGUAGE TypeFamilies #-}
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

instance Kahn IO where
    type InChannel  IO = Handle1
    type OutChannel IO = Handle1

    newChannel = do
        -- On crée une socket TCP
        sock <- socket AF_INET Stream defaultProtocol
        setSocketOption sock ReuseAddr 1
        -- On la lie à un port aléatoire sur localhost
        bind sock (SockAddrInet defaultPort localhost)
        -- On récupère le port assigné par l'OS
        port <- socketPort sock
        -- On écoute les connections
        listen sock 1024
        -- On crée un thread chargé d'accepter une connection et de placer la socket
        -- résultante dans `v`
        v <- newEmptyMVar
        forkIO $ putMVar v . fst =<< accept sock
        -- En parallèle, on se connecte
        client <- socket AF_INET Stream defaultProtocol
        connect client (SockAddrInet port localhost)
        -- On récupère la socket du serveur
        server <- takeMVar v
        -- On transforme les sockets en `Handle`. On choisit ici de lire depuis
        -- la socket "serveur" et d'écrire sur la socket "client", mais on aurait
        -- très bien pu faire l'inverse.
        i <- socketToHandle server ReadMode
        o <- socketToHandle client WriteMode
        return (H i, H o)
        where localhost = tupleToHostAddress (127, 0, 0, 1)

    -- cf. KahnProcesses.hs
    put (H h) a = hPutStr h (show a ++ "\0")
    get (H h)   = fmap read $ many $ mfilter (/= '\0') (hGetChar h)

    -- cf. KahnThreads.hs
    doco as = mapM_ takeMVar =<< mapM fork as
        where
        fork a = do
            v <- newEmptyMVar
            forkFinally a (\_ -> putMVar v ())
            return v

    run = id
