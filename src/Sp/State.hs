module Sp.State where

import           Data.Functor      (($>))
import           Data.IORef        (IORef, readIORef, writeIORef)
import           Sp.Eff
import GHC.IORef (newIORef)
import Sp.Internal.Monad (unsafeIO)
import Sp.Reader (runReader, ask)


-- | Provides a mutable state of type @s@.
data State s :: Effect where
  Get :: State s m s
  Put :: s -> State s m ()
  State :: (s -> (s, a)) -> State s m a

-- | Get the mutable state.
get :: State s :> es => Eff es s
get = send Get

-- | Write a new value to the mutable state.
put :: State s :> es => s -> Eff es ()
put x = send (Put x)

-- | Apply a function to the mutable state.
modify :: State s :> es => (s -> s) -> Eff es ()
modify f = state ((, ()) . f)

-- | Apply a function of type @s -> (s, a)@ on the mutable state, using the returned @s@ as the new state and
-- returning the @a@.
state :: State s :> es => (s -> (s, a)) -> Eff es a
state f = send (State f)

handleState :: IORef s -> Handler (State s) es a
handleState r _ = \case
  Get     -> unsafeIO $ readIORef r
  Put s   -> unsafeIO $ writeIORef r s
  State f -> unsafeIO do
    (!s1, x) <- f <$> readIORef r
    writeIORef r s1 $> x

-- | Run the 'State' effect with an initial value for the mutable state.
runStateIO :: s -> Eff (State s : es) a -> Eff es (a, s)
runStateIO s m = do
  r <- unsafeIO $ newIORef s
  x <- interpret (handleState r) m
  s' <- unsafeIO (readIORef r)
  pure (x, s')


runStatePure :: s -> Eff (State s ': ef) a -> Eff ef (a, s)
runStatePure s m = runReader s . interpret0 (\tag -> \case
        Get -> embed tag ask
        Put s' -> control tag \k -> lift1 $ runReader s' $ k $ pure ()
    ) $ lift1Under1 do
        r <- m
        s' <- get
        pure (r,s')
