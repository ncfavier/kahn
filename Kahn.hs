{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Kahn where

import Control.Monad.IO.Class

class MonadIO m => Kahn m i o | m -> i o where
    newChannel :: m (i a, o a)
    put        :: Show a => o a -> a -> m ()
    get        :: Read a => i a -> m a

    doco       :: [m ()] -> m ()

    run        :: m a -> IO a
