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
get :: (State s :> es) => Eff es s
get = send Get

-- | Write a new value to the mutable state.
put :: (State s :> es) => s -> Eff es ()
put x = send (Put x)

-- | Apply a function to the mutable state.
modify :: (State s :> es) => (s -> s) -> Eff es ()
modify f = state ((,()) . f)

{- | Apply a function of type @s -> (s, a)@ on the mutable state, using the returned @s@ as the new state and
 returning the @a@.
-}
state :: (State s :> es) => (s -> (s, a)) -> Eff es a
state f = do
    s <- get
    let (s', a) = f s
    put s'
    pure a

handleState :: IORef s -> Handler (State s) es a
handleState ref _ = \case
    Get -> liftIO $ readIORef ref
    Put s -> liftIO $ writeIORef ref s

-- | Run the 'State' effect with an initial value for the mutable state.
runState :: forall s es a. s -> Eff (State s : es) a -> Eff es (a, s)
runState s0 m = do
    ref <- liftIO $ newIORef s0
    interpret0 (handleState ref) do
        r <- liftUnder1 m
        s <- get
        pure (r, s)
