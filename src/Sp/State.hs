module Sp.State where

import           Data.Functor      (($>))
import           Data.IORef        (IORef, readIORef, writeIORef)
import           Sp.Eff
import GHC.IORef (newIORef)
import Sp.Reader


-- | Provides a mutable state of type @s@.
data State s :: EffectF where
  Get :: State s s
  Put :: s -> State s ()

-- | Get the mutable state.
get :: State s :> ef => Eff eh ef s
get = send Get

-- | Write a new value to the mutable state.
put :: State s :> ef => s -> Eff eh ef ()
put x = send (Put x)

-- | Apply a function to the mutable state.
modify :: State s :> ef => (s -> s) -> Eff eh ef ()
modify f = state ((, ()) . f)

-- | Apply a function of type @s -> (s, a)@ on the mutable state, using the returned @s@ as the new state and
-- returning the @a@.
state :: State s :> ef => (s -> (s, a)) -> Eff eh ef a
state f = do
    (!s,x) <- f <$> get
    put s $> x

handleStateIO :: IORef s -> Handler (State s) eh ef a
handleStateIO r _ = \case
  Get     -> unsafeIO $ readIORef r
  Put s   -> unsafeIO $ writeIORef r s

-- | Run the 'State' effect with an initial value for the mutable state.
runStateIO :: s -> Eff eh (State s : ef) a -> Eff eh ef (a, s)
runStateIO s m = do
  r <- unsafeIO $ newIORef s
  x <- interpretRec (handleStateIO r) m
  s' <- unsafeIO (readIORef r)
  pure (x, s')

-- todo: runStatePure


runStatePureRec :: s -> Eff eh (State s ': ef) a -> Eff eh ef (a, s)
runStatePureRec s m = runAsk s . interpretRec0 (\tag -> \case
        Get -> embed tag $ send Ask
        Put s' -> control tag \k -> lift1 $ runAsk s' $ k $ pure ()
    ) $ lift1Under1 do
        r <- m
        s' <- get
        pure (r,s')

runStatePure :: s -> Eff '[] (State s ': ef) a -> Eff '[] ef (a, s)
runStatePure s m = runAsk s . interpret0 (\tag -> \case
        Get -> embed tag $ send Ask
        Put s' -> control tag \k -> lift1 $ runAsk s' $ k $ pure ()
    ) $ lift1Under1 do
        r <- m
        s' <- get
        pure (r,s')
