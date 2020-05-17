import Control.Monad
import Control.Monad.IO.Class
import System.IO
import Text.Printf

import Kahn

import KahnThreads
-- import KahnProcesses
-- import KahnSockets
-- import KahnSequential

main = do
    hSetBuffering stdout NoBuffering
    run printIntegers
    -- run primes
    -- run pingpong

-- Envoie les entiers à partir de 2 dans le canal `o`
integers o = put o `mapM_` [2..]

-- Lit depuis le canal `i` et affiche sur la sortie standard
output i = forever (liftIO . print =<< get i)

-- Affiche les entiers à partir de 2
printIntegers :: Process ()
printIntegers = do
    (i, o) <- newChannel
    doco [integers o, output i]

-- Affiche la suite des nombres premiers en utilisant le crible d'Ératosthène
primes :: Process ()
primes = do
    (i1, o1) <- newChannel
    (i2, o2) <- newChannel
    doco [integers o1, sift i1 o2, output i2]
    where
    sift i o = do
        p <- get i
        put o p
        (i', o') <- newChannel
        doco [filterIntegers p i o', sift i' o]
    filterIntegers p i o = forever $ do
        n <- get i
        when (n `mod` p /= 0) (put o n)

-- Ping? Pong.
pingpong :: Process ()
pingpong = do
    (i1, o1) <- newChannel
    (i2, o2) <- newChannel
    doco [proc "ping" i1 o2, proc "pong" i2 o1]
    where
    proc msg i o = forever $ do
        liftIO $ printf "%s: put\n" msg
        put o 1
        liftIO $ printf "%s: get\n" msg
        get i
