-- |
-- Copyright: (c) 2022 Xy Ren
-- License: BSD3
-- Maintainer: xy.r@outlook.com
-- Stability: experimental
-- Portability: non-portable (GHC only)
--
-- Delimited control monad based on the design of Xie et al. This implementation imposes much less cost (albeit still
-- noticeable) on computations not utilizing delimited control than the naive implementation. It also doesn't rely on
-- compiler-supplied primitive operations.
module Sp.Internal.Ctl.Monadic
  ( Marker
  , Ctl
  , freshMarker
  ) where

import           Control.Exception      (MaskingState (MaskedInterruptible, MaskedUninterruptible, Unmasked),
                                         SomeException, getMaskingState)
import qualified Control.Exception      as Exception
import           Control.Monad          (ap, liftM, (<=<))
import           Control.Monad.Catch    (MonadCatch, MonadThrow)
import qualified Control.Monad.Catch    as Catch
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.IORef             (IORef, readIORef, writeIORef)
import           Data.Kind              (Type)
import           Data.Primitive.PrimVar (PrimVar, fetchAddInt, newPrimVar)
import           Data.Type.Equality     (type (:~:) (Refl))
import           GHC.Exts               (RealWorld, maskAsyncExceptions#, maskUninterruptible#, unmaskAsyncExceptions#)
import           GHC.IO                 (IO (IO))
import           System.IO.Unsafe       (unsafePerformIO)
import           Unsafe.Coerce          (unsafeCoerce)

-- | The source from which we construct unique 'Marker's.
uniqueSource :: PrimVar RealWorld Int
uniqueSource = unsafePerformIO (newPrimVar 0)
{-# NOINLINE uniqueSource #-}

-- | Create a fresh 'Marker'.
freshMarker :: ∀ h a e. (Marker h a e -> Ctl e a) -> Ctl e a
freshMarker f =
    let mark = unsafePerformIO $ fetchAddInt uniqueSource 1
    in f $ Marker mark
{-# NOINLINE freshMarker #-}

-- | A @'Marker' a@ marks a prompt frame over a computation returning @a@.
type role Marker nominal nominal representational
newtype Marker (h :: Type -> Type -> Type) (e :: Type)  (a :: Type) = Marker Int

-- | Check the equality of two markers, and if so provide a proof that the type parameters are equal. This does not
-- warrant a @TestEquality@ instance because it requires decidable equality over the type parameters.
eqMarker :: Marker h e a -> Marker h' e' b -> Maybe ((h e a, a, e) :~: (h' e' b, b, e'))
eqMarker (Marker l) (Marker r) =
  if l == r then Just (unsafeCoerce Refl) else Nothing

-- | The delimited control monad, with efficient support of tail-resumptive computations.
type role Ctl nominal representational
data Ctl e (a :: Type)
  = Pure a
  | ∀ (r :: Type) (b :: Type) h e'.
        Control !(Marker h e' r) ((b -> Eff e' r) -> Eff e' r) (b -> Eff e a)

newtype Eff e a = Eff {unEff :: ctx -> Ctl e a}

-- | Extend the captured continuation with a function, if it exists.
extend :: (Eff ctx a -> Eff ctx a) -> Ctl ctx a -> Ctl ctx a
extend f = \case
  Pure a                -> Pure a
  Control mark ctl cont -> Control mark ctl (f . cont)

instance Functor (Eff ctx) where
  fmap = liftM

instance Applicative (Eff ctx) where
  pure x = Eff \_ -> Pure x
  (<*>) = ap

instance Monad (Eff ctx) where
  Eff eff >>= f =
        Eff \ctx -> case eff ctx of
            Pure x -> unEff (f x) ctx
            Control mark ctl cont -> Control mark ctl (f `compose` cont)
  {-# INLINE (>>=) #-}

-- | This loopbreaker is crucial to the performance of the monad.
compose :: (b -> Eff ctx c) -> (a -> Eff ctx b) -> a -> Eff ctx c
compose = (<=<)
{-# NOINLINE compose #-}

-- use a prompt with a unique marker (and handle yields to it)
{-# INLINE prompt #-}
prompt :: Marker h e ans -> h e ans -> Eff (h :* e) ans -> Eff e ans
prompt m h (Eff eff) = Eff $ \ctx ->
    case (eff (CCons m h ctx)) of -- add handler to the context
        Pure x -> Pure x
        Control n op cont ->
            let cont' x = prompt m h (cont x) -- extend the continuation with our own prompt
             in case mmatch m n of
                    Nothing -> Control n op cont' -- keep yielding (but with the extended continuation)
                    Just Refl -> unEff (op cont') ctx -- found our prompt, invoke `op` (under the context `ctx`).
                    -- Note: `Refl` proves `a ~ ans` and `e ~ e'` (the existential `ans,e'` in Control)


{-
-- | Install a prompt frame.
prompt, prompt' :: Marker a -> Ctl a -> Ctl a
prompt !mark (Ctl m) = Ctl $ m >>= \case
  Pure a -> pure $ Pure a
  Abort mark' r -> case eqMarker mark mark' of
    Just Refl -> unCtl r
    Nothing   -> pure $ Abort mark' r
  Control mark' ctl cont -> case eqMarker mark mark' of
    Just Refl -> unCtl $ ctl (prompt' mark . cont)
    Nothing   -> pure $ Control mark' ctl (prompt' mark . cont)
{-# INLINE prompt #-}
prompt' = prompt
{-# NOINLINE prompt' #-}

-- | Capture the resumption up to and including the prompt frame specified by the 'Marker'.
control :: Marker r -> ((Ctl a -> Ctl r) -> Ctl r) -> Ctl a
control !mark f = Ctl $ pure $ Control mark f id

-- | Replace the current computation up to and including the prompt with a new one.
abort :: Marker r -> Ctl r -> Ctl a
abort !mark r = Ctl $ pure $ Abort mark r

-- | Introduce a mutable state that behaves well wrt reentry.
promptState, promptState' :: IORef s -> Ctl r -> Ctl r
promptState !ref (Ctl m) = Ctl $ m >>= \case
  Pure x -> pure $ Pure x
  Abort mark x -> pure $ Abort mark x
  Control mark ctl cont -> do
    s0 <- liftIO (readIORef ref)
    pure $ Control mark ctl \x -> do
      liftIO (writeIORef ref s0)
      promptState' ref (cont x)
{-# INLINE promptState #-}
promptState' = promptState
{-# NOINLINE promptState' #-}

-- | Unwrap the 'Ctl' monad.
runCtl :: Ctl a -> IO a
runCtl (Ctl m) = m >>= \case
  Pure a     -> pure a
  Abort {}   -> error "Sp.Ctl: Unhandled abort operation. Forgot to pair it with a prompt?"
  Control {} -> error "Sp.Ctl: Unhandled control operation. Forgot to pair it with a prompt?"

instance MonadIO Ctl where
  liftIO = Ctl . fmap Pure

instance MonadThrow Ctl where
  throwM = Ctl . Exception.throwIO

-- | Note that although both catching and masking are possible, implementing 'Catch.generalBracket' via them will not
-- be well-behaved wrt reentry; hence 'Ctl' is not 'Catch.MonadMask'.
instance MonadCatch Ctl where
  catch m h = liftMap (Exception.handle (unCtl . h)) m

-- | Install pre- and post-actions that are well-behaved wrt reentry. Specifically, pre- and post-actions are always
-- guaranteed to act in pairs.
dynamicWind, dynamicWind' :: Ctl () -> Ctl () -> Ctl a -> Ctl a
dynamicWind before after (Ctl action) = do
  res <- before >> Ctl do
    res <- Exception.try @SomeException action
    pure $ Pure res
  after >> Ctl case res of
    Left se -> Exception.throwIO se
    Right y -> pure $ extend (dynamicWind' before after) y
{-# INLINE dynamicWind #-}
dynamicWind' = dynamicWind
{-# NOINLINE dynamicWind' #-}

block, unblock, blockUninterruptible :: Ctl a -> Ctl a
block = liftMap \(IO m) -> IO $ maskAsyncExceptions# m
unblock = liftMap \(IO m) -> IO $ unmaskAsyncExceptions# m
blockUninterruptible = liftMap \(IO m) -> IO $ maskUninterruptible# m

-- | Lifted version of 'Exception.mask'.
mask :: ((∀ x. Ctl x -> Ctl x) -> Ctl a) -> Ctl a
mask io = liftIO getMaskingState >>= \case
  Unmasked              -> block $ io unblock
  MaskedInterruptible   -> io block
  MaskedUninterruptible -> io blockUninterruptible

-- | Lifted version of 'Exception.uninterruptibleMask'.
uninterruptibleMask :: ((∀ x. Ctl x -> Ctl x) -> Ctl a) -> Ctl a
uninterruptibleMask io = liftIO getMaskingState >>= \case
  Unmasked              -> blockUninterruptible $ io unblock
  MaskedInterruptible   -> blockUninterruptible $ io block
  MaskedUninterruptible -> io blockUninterruptible

-- | Lifted version of 'Exception.interruptible'.
interruptible :: Ctl a -> Ctl a
interruptible io = liftIO getMaskingState >>= \case
  Unmasked              -> io
  MaskedInterruptible   -> unblock io
  MaskedUninterruptible -> io
-}
