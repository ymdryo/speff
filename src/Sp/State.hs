module Sp.State where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Sp.Eff
import Sp.Reader

-- | Provides a mutable state of type @s@.
data State s :: Effect where
    Get :: State s s
    Put :: s -> State s ()

-- | Get the mutable state.
get :: (State s :> es, Monad m) => Eff m es s
get = send Get

-- | Write a new value to the mutable state.
put :: (State s :> es, Monad m) => s -> Eff m es ()
put x = send (Put x)

-- | Apply a function to the mutable state.
modify :: (State s :> es, Monad m) => (s -> s) -> Eff m es ()
modify f = state ((,()) . f)

{- | Apply a function of type @s -> (s, a)@ on the mutable state, using the returned @s@ as the new state and
 returning the @a@.
-}
state :: (State s :> es, Monad m) => (s -> (s, a)) -> Eff m es a
state f = do
    s <- get
    let (s', a) = f s
    put s'
    pure a

handleState :: MonadIO m => IORef s -> Handler m (State s) es a
handleState ref _ = \case
    Get -> liftIO $ readIORef ref
    Put s -> liftIO $ writeIORef ref s

-- | Run the 'State' effect with an initial value for the mutable state.
runState :: forall s es m a. MonadIO m => s -> Eff m (State s : es) a -> Eff m es (a, s)
runState s0 m = do
    ref <- liftIO $ newIORef s0
    interpret0 (handleState ref) do
        r <- liftUnder1 m
        s <- get
        pure (r, s)
