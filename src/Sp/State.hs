module Sp.State where

import Data.Functor (($>))
import Data.IORef (IORef, readIORef, writeIORef)
import Sp.Eff
import Sp.Internal.Monad (HandleTag (HandleTag))

-- | Provides a mutable state of type @s@.
data State s :: EffectH where
    Get :: State s m s
    Put :: s -> State s m ()
