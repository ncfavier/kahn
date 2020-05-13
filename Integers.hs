{-# LANGUAGE TypeApplications #-}
module Integers where

import Control.Monad
import Control.Monad.IO.Class

import Kahn
import KahnSequential -- remplacer par l'implémentation souhaitée

integers qOut = put qOut `mapM_` [2..]

output qIn = forever (liftIO . print =<< get qIn)

main = run @Process $ do
    (qIn, qOut) <- newChannel
    doco [integers qOut, output qIn]
