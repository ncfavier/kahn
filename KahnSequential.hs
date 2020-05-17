{-# LANGUAGE TypeFamilies #-}
module KahnSequential where

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef

import Kahn

-- Implémentation classique des files avec deux listes
data Queue a = Queue [a] [a]

emptyQueue = Queue [] []

-- On ajoute à la liste d'entrée
push x (Queue i o) = Queue (x:i) o

-- On lit depuis la liste de sortie. Si elle est vide, on
-- renverse la liste d'entrée dans la liste de sortie.
pop (Queue i (x:o)) = Just (x, Queue i o)
pop (Queue [] [])   = Nothing
pop (Queue i [])    = pop (Queue [] (reverse i))

-- Fournit une interface mutable pour nos files
newtype IOQueue a = Q (IORef (Queue a))

newIOQueue = Q <$> newIORef emptyQueue

pushIOQueue (Q q) x = modifyIORef q (push x)

popIOQueue (Q q) = do
    m <- pop <$> readIORef q
    case m of
        Nothing -> return Nothing
        Just (x, q') -> Just x <$ writeIORef q q'

-- La monade libre pour `IO` (cf. LISEZMOI)
data Process a = Pure a | Atom (IO (Process a))

-- `Functor` est une super-classe de `Monad`, on se contente de fournir
-- l'instance "par défaut"
instance Functor Process where
    fmap = liftM

-- idem.
instance Applicative Process where
    pure = Pure
    (<*>) = ap

-- Analogue à `Monoid [a]`.
instance Monad Process where
    return = Pure
    Pure x >>= f = f x
    Atom m >>= f = Atom ((>>= f) <$> m)

-- `liftIO` enveloppe une action `IO` dans un processus atomique.
instance MonadIO Process where
    liftIO m = Atom (Pure <$> m)

instance Kahn Process where
    type InChannel  Process = IOQueue
    type OutChannel Process = IOQueue

    newChannel = (\q -> (q, q)) <$> liftIO newIOQueue
    put q a    = liftIO (pushIOQueue q a)
    -- On essaye de lire de manière atomique. S'il n'y a rien à lire,
    -- on réessaye récursivement.
    get q      = maybe (get q) return =<< liftIO (popIOQueue q)

    -- Rien à faire.
    doco [] = return ()
    -- On exécute les actions atomiques en tête, puis on passe la nouvelle
    -- liste à `doco` récursivement.
    doco as = doco =<< sequence [liftIO m | Atom m <- as]

    -- On exécute les actions séquentiellement dans la monade `IO`.
    run (Pure x) = return x
    run (Atom m) = run =<< m
