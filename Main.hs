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
    runKahn printIntegers
    -- runKahn primes
    -- runKahn lockstep

integers o = put o `mapM_` [2..]

output i = forever (liftIO . print =<< get i)

printIntegers :: Process ()
printIntegers = do
    (i, o) <- newChannel
    doco [integers o, output i]

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

lockstep :: Process ()
lockstep = do
    (i1, o1) <- newChannel
    (i2, o2) <- newChannel
    doco [proc "A" i1 o2, proc "B" i2 o1]
    where
    proc msg i o = forever $ do
        liftIO $ printf "%s: PUT\n" msg
        put o 1
        liftIO $ printf "%s: GET\n" msg
        get i
