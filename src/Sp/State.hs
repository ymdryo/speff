module Sp.State where

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

handleState :: Monad m => Handler m (State s) (Reader s ': es) a
handleState tag = \case
    Get ->
        embed tag ask
    Put s ->
        control tag \k -> do
            lift1 $ runReader s $ k ()

-- | Run the 'State' effect with an initial value for the mutable state.
runState :: forall s es m a. Monad m => s -> Eff m (State s : es) a -> Eff m es (a, s)
runState s0 m =
    runReader s0 do
        interpret0 handleState do
            r <- liftUnder1 m
            s <- ask
            pure (r, s)
