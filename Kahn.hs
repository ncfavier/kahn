{-# LANGUAGE TypeFamilies #-}
module Kahn where

import Control.Monad.IO.Class

class MonadIO m => Kahn m where
    type InChannel m  :: * -> *
    type OutChannel m :: * -> *

    newChannel :: m (InChannel m a, OutChannel m a)
    put        :: Show a => OutChannel m a -> a -> m ()
    get        :: Read a => InChannel m a -> m a

    doco       :: [m ()] -> m ()

    runKahn    :: m a -> IO a
