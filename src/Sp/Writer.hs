module Sp.Writer where

import           Data.Foldable     (for_)
import           Data.IORef        (IORef, modifyIORef', readIORef, newIORef)
import           Data.Kind         (Type)
import           Sp.Eff
import           Sp.Internal.Monad (unsafeIO)

-- | Provides an append-only state, and also allows you to record what is appended in a specific scope.
data Tell (w :: Type) :: EffectF where
  Tell :: w -> Tell w ()

data WriterH (w :: Type) :: EffectH where
  Listen :: m a -> WriterH w m (a, w)

-- | Append a value to the state.
tell :: Tell w :> ef => w -> Eff eh ef ()
tell x = send (Tell x)

-- | Record what is appended in a specific scope.
listen :: WriterH w :> eh => Eff eh ef a -> Eff eh ef (a, w)
listen m = sendH (Listen m)

{-
handleWriter :: ∀ w es a. Monoid w => [IORef w] -> Handler (Writer w) es a
handleWriter rs _ = \case
  Tell x   -> for_ rs \r -> unsafeIO (modifyIORef' r (<> x))
  Listen m -> do
    r <- unsafeIO $ newIORef mempty
    x <- replace (handleWriter $ r : rs) m
    w' <- unsafeIO (readIORef r)
    pure (x, w')
{-# INLINABLE handleWriter #-}

-- | Run the 'Writer' state, with the append-only state as a monoidal value.
runWriter :: Monoid w => Eff (Writer w : es) a -> Eff es (a, w)
runWriter m = do
  r <- unsafeIO $ newIORef mempty
  x <- interpret (handleWriter [r]) m
  w' <- unsafeIO (readIORef r)
  pure (x, w')
{-# INLINABLE runWriter #-}
-}
