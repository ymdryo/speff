{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
-- |
-- Copyright: (c) 2022 Xy Ren
-- License: BSD3
-- Maintainer: xy.r@outlook.com
-- Stability: experimental
-- Portability: non-portable (GHC only)
--
-- The effect monad, along with handling combinators that enable delimited control and higher-order effects.
module Sp.Internal.Monad where

import           Control.Monad          (ap, liftM)
import           Control.Monad.Catch    (MonadCatch, MonadThrow)
import qualified Control.Monad.Catch    as Catch
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.IORef             (IORef, newIORef, readIORef)
import           Data.Kind              (Type)
import qualified Sp.Internal.Env        as Rec
import           Sp.Internal.Env        (Rec, (:>))
import           System.IO.Unsafe       (unsafePerformIO)
import qualified Control.Exception      as Exception
import           Control.Monad          ((<=<))
import           Data.Primitive.PrimVar (PrimVar, fetchAddInt, newPrimVar)
import           Data.Type.Equality     (type (:~:) (Refl))
import           GHC.Exts               (RealWorld)
import           Unsafe.Coerce          (unsafeCoerce)

-- | The source from which we construct unique 'Marker's.
uniqueSource :: PrimVar RealWorld Int
uniqueSource = unsafePerformIO (newPrimVar 0)
{-# NOINLINE uniqueSource #-}

-- | Create a fresh 'Marker'.
freshMarker :: ∀ a. Ctl (Marker a)
freshMarker = liftIO $ Marker <$> fetchAddInt uniqueSource 1

-- | A @'Marker' a@ marks a prompt frame over a computation returning @a@.
type role Marker representational
newtype Marker (a :: Type) = Marker Int

-- | Check the equality of two markers, and if so provide a proof that the type parameters are equal. This does not
-- warrant a @TestEquality@ instance because it requires decidable equality over the type parameters.
eqMarker :: Marker a -> Marker b -> Maybe (a :~: b)
eqMarker (Marker l) (Marker r) =
  if l == r then Just (unsafeCoerce Refl) else Nothing

-- | Intermediate result of a `Ctl` computation.
type role Result representational
data Result (a :: Type)
  = Pure a
  -- ^ The computation returned normally.
  | ∀ (r :: Type). Abort !(Marker r) (Ctl r)
  -- ^ The computation replaced itself with another computation.
  | ∀ (r :: Type) (b :: Type). Control !(Marker r) ((Ctl b -> Ctl r) -> Ctl r) (Ctl b -> Ctl a)
  -- ^ The computation captured a resumption and gained control over it. Specifically, this uses @shift0@ semantics.

-- | Extend the captured continuation with a function, if it exists.
extend :: (Ctl a -> Ctl a) -> Result a -> Result a
extend f = \case
  Pure a                -> Pure a
  Abort mark r          -> Abort mark r
  Control mark ctl cont -> Control mark ctl (f . cont)

-- | The delimited control monad, with efficient support of tail-resumptive computations.
type role Ctl representational
newtype Ctl (a :: Type) = Ctl { unCtl :: IO (Result a) }

instance Functor Ctl where
  fmap = liftM

instance Applicative Ctl where
  pure = Ctl . pure . Pure
  (<*>) = ap

instance Monad Ctl where
  (Ctl x) >>= f = Ctl $ x >>= \case
    Pure a                -> unCtl (f a)
    Abort mark r          -> pure $ Abort mark r
    Control mark ctl cont -> pure $ Control mark ctl (f `compose` cont)

-- | This loopbreaker is crucial to the performance of the monad.
compose :: (b -> Ctl c) -> (a -> Ctl b) -> a -> Ctl c
compose = (<=<)
{-# NOINLINE compose #-}

-- | Lift an 'IO' function to a 'Ctl' function. The function must not alter the result.
liftMap, liftMap' :: (IO (Result a) -> IO (Result a)) -> Ctl a -> Ctl a
liftMap f (Ctl m) = Ctl $ extend (liftMap' f) <$> f m
{-# INLINE liftMap #-}
liftMap' = liftMap
{-# NOINLINE liftMap' #-}

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

-- | Unwrap the 'Ctl' monad.
runCtl :: Ctl a -> IO a
runCtl (Ctl m) = m >>= \case
  Pure a     -> pure a
  Abort {}   -> error "Sp.Ctl: Unhandled abort operation. Forgot to pair it with a prompt?"
  Control {} -> error "Sp.Ctl: Unhandled control operation. Forgot to pair it with a prompt?"


-- | The kind of higher-order effects, parameterized by (1) the monad in which it was performed, and (2) the result
-- type.
type Effect = (Type -> Type) -> Type -> Type

-- | The concrete representation of an effect context: a record of internal handler representations.
type Env = Rec HandlerCell

newtype HandlerCell (e :: Effect) = HandlerCell { getHandlerCell :: InternalHandler e }

-- | The effect monad; it is parameterized by the /effect context/, i.e. a row of effects available. This monad is
-- implemented with evidence passing and a delimited control monad with support of efficient tail-resumptive
-- (non-capturing) computations and @IO@ embedding.
type role Eff nominal representational
newtype Eff (es :: [Effect]) (a :: Type) = Eff { unEff :: Env es -> Ctl a }

-- | The internal representation of a handler of effect @e@. This representation is only valid within the original
-- context in which the effect was introduced.
type role InternalHandler nominal
newtype InternalHandler e = InternalHandler
  { runHandler :: ∀ es a. e :> es => e (Eff es) a -> Eff es a }

instance Functor (Eff es) where
  fmap = liftM
  {-# INLINE fmap #-}

instance Applicative (Eff es) where
  pure x = Eff \_ -> pure x
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Monad (Eff es) where
  Eff m >>= f = Eff \es -> m es >>= \x -> unEff (f x) es
  {-# INLINE (>>=) #-}

-- | The tag associated to a handler that was /introduced/ in context @es@ over an computation with
-- /eventual result type/ @r@. Value of this type enables delimited control and scoped effects.
data HandleTag (tag :: Type) (es :: [Effect]) (r :: Type) = HandleTag (Env es) !(Marker r)

-- | A handler of effect @e@ introduced in context @es@ over a computation returning @r@.
type Handler e es r = ∀ tag esSend a. e :> esSend => HandleTag tag es r -> e (Eff esSend) a -> Eff (Handling tag : esSend) a

-- | This "unsafe" @IO@ function is perfectly safe in the sense that it won't panic or otherwise cause undefined
-- behaviors; it is only unsafe when it is used to embed arbitrary @IO@ actions in any effect environment,
-- therefore breaking effect abstraction.
unsafeIO :: IO a -> Eff es a
unsafeIO m = Eff (const $ liftIO m)
{-# INLINE unsafeIO #-}

-- | Convert an effect handler into an internal representation with respect to a certain effect context and prompt
-- frame.
toInternalHandler :: ∀ e es r. Marker r -> Env es -> Handler e es r -> InternalHandler e
toInternalHandler mark es hdl = InternalHandler \e -> alter Rec.pad $ hdl (HandleTag @() es mark) e

-- | Do a trivial transformation over the effect context.
alter :: (Env es' -> Env es) -> Eff es a -> Eff es' a
alter f = \(Eff m) -> Eff \es -> m $! f es
{-# INLINE alter #-}

-- | General effect handling. Introduce a prompt frame, convert the supplied handler to an internal one wrt that
-- frame, and then supply the internal handler to the given function to let it add that to the effect context.
handle :: (HandlerCell e -> Env es' -> Env es) -> Handler e es' a -> Eff es a -> Eff es' a
handle f = \hdl (Eff m) -> Eff \es -> do
  mark <- freshMarker
  let cell = toInternalHandler mark es hdl
  prompt mark $ m $! f (HandlerCell cell) es
{-# INLINE handle #-}

-- | Perform an effect operation.
send :: e :> es => e (Eff es) a -> Eff es a
send e = Eff \es -> do
  let ih = getHandlerCell $ Rec.index es
  unEff (runHandler ih e) es
{-# INLINE send #-}

-- | A "localized computaton"; this should be parameterized with an existential variable so the computation with this
-- effect cannot escape a certain scope.
data Localized (tag :: Type) :: Effect
data Handling (tag :: Type) :: Effect

-- | Perform an operation from the handle-site.
embed :: Handling tag :> esSend => HandleTag tag es r -> Eff es a -> Eff esSend a
embed (HandleTag es _) (Eff m) = Eff \_ -> m es
{-# INLINE embed #-}

-- | Abort with a result value.
abort :: Handling tag :> esSend => HandleTag tag es r -> Eff es r -> Eff esSend a
abort (HandleTag es mark) (Eff m) = Eff \_ -> Ctl $ pure $ Abort mark (m es)
{-# INLINE abort #-}

-- | Capture and gain control of the resumption. The resumption cannot escape the scope of the controlling function.
control
  :: Handling tag :> esSend
  => HandleTag tag es r
  -> (∀ tag'. (Eff esSend a -> Eff (Localized tag' : es) r) -> Eff (Localized tag' : es) r)
  -> Eff esSend a
control (HandleTag es mark) f =
  Eff \esSend -> Ctl $ pure $ Control mark (\cont -> unEff (f \(Eff x) -> Eff \_ -> cont $ x esSend) $! Rec.pad es) id
{-# INLINE control #-}

-- | Unwrap the 'Eff' monad.
runEff :: Eff '[] a -> a
runEff (Eff m) = unsafePerformIO (runCtl $ m Rec.empty)
{-# INLINE runEff #-}

-- | Ability to embed 'IO' side effects.
data IOE :: Effect

instance IOE :> es => MonadIO (Eff es) where
  liftIO = unsafeIO
  {-# INLINE liftIO #-}

instance MonadThrow (Eff es) where
  throwM x = Eff \_ -> Catch.throwM x
  {-# INLINE throwM #-}

instance IOE :> es => MonadCatch (Eff es) where
  catch (Eff m) h = Eff \es -> Catch.catch (m es) \ex -> unEff (h ex) es
  {-# INLINE catch #-}

-- | Unwrap an 'Eff' monad with 'IO' computations.
runIOE :: Eff '[IOE] a -> IO a
runIOE m = runCtl $ unEff m (Rec.pad Rec.empty)
{-# INLINE runIOE #-}


instance MonadIO Ctl where
  liftIO = Ctl . fmap Pure

instance MonadThrow Ctl where
  throwM = Ctl . Exception.throwIO

-- | Note that although both catching and masking are possible, implementing 'Catch.generalBracket' via them will not
-- be well-behaved wrt reentry; hence 'Ctl' is not 'Catch.MonadMask'.
instance MonadCatch Ctl where
  catch m h = liftMap (Exception.handle (unCtl . h)) m
