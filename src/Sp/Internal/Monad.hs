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
import           Data.Kind              (Type)
import qualified Sp.Internal.Env        as Rec
import           Sp.Internal.Env        (Rec, (:>))
import           System.IO.Unsafe       (unsafePerformIO)
import qualified Control.Exception      as Exception
import           Data.Primitive.PrimVar (PrimVar, fetchAddInt, newPrimVar)
import           Data.Type.Equality     (type (:~:) (Refl))
import           GHC.Exts               (RealWorld)
import           Unsafe.Coerce          (unsafeCoerce)
import Data.Functor ((<&>))

-- | The source from which we construct unique 'Marker's.
uniqueSource :: PrimVar RealWorld Int
uniqueSource = unsafePerformIO (newPrimVar 0)
{-# NOINLINE uniqueSource #-}

-- | Create a fresh 'Marker'.
freshMarker :: Ctl es (Marker e es' a)
freshMarker = liftIO $ Marker <$> fetchAddInt uniqueSource 1

-- | A @'Marker' a@ marks a prompt frame over a computation returning @a@.
type role Marker nominal nominal representational
newtype Marker (e :: Effect) (es :: [Effect]) (a :: Type) = Marker Int

-- | Check the equality of two markers, and if so provide a proof that the type parameters are equal. This does not
-- warrant a @TestEquality@ instance because it requires decidable equality over the type parameters.
eqMarker :: Marker e es a -> Marker e' es' b -> Maybe ((a,e m a) :~: (b, e' m b), es :~: es')
eqMarker (Marker l) (Marker r) =
  if l == r then Just (unsafeCoerce Refl, unsafeCoerce Refl) else Nothing

-- | Intermediate result of a `Ctl` computation.
type role Result nominal representational
data Result es (a :: Type)
  = Pure a
  -- ^ The computation returned normally.
  | ∀ (r :: Type) e es'. Abort !(Marker e es' r) (Ctl es' r)
  -- ^ The computation replaced itself with another computation.
  | ∀ (r :: Type) e es' (b :: Type). Control !(Marker e es' r) ((b -> Eff es' r) -> Eff es' r) (b -> Eff es a)
  -- ^ The computation captured a resumption and gained control over it. Specifically, this uses @shift0@ semantics.

-- | Extend the captured continuation with a function, if it exists.
extend :: (Ctl es a -> Ctl es a) -> Result es a -> Result es a
extend f = \case
  Pure a                -> Pure a
  Abort mark r          -> Abort mark r
  Control mark ctl cont -> Control mark ctl ((\(Eff m) -> Eff \es -> f $ m es) . cont)

-- | The delimited control monad, with efficient support of tail-resumptive computations.
type role Ctl nominal representational
newtype Ctl es (a :: Type) = Ctl { unCtl :: IO (Result es a) }

instance Functor (Ctl es) where
  fmap = liftM

instance Applicative (Ctl es) where
  pure = Ctl . pure . Pure
  (<*>) = ap

instance Monad (Ctl es) where
  (Ctl x) >>= f = Ctl $ x >>= \case
    Pure a                -> unCtl (f a)
    Abort mark r          -> pure $ Abort mark r
    Control mark ctl cont -> pure $ Control mark ctl (f `compose` cont)

-- | This loopbreaker is crucial to the performance of the monad.
compose :: (b -> Ctl es c) -> (a -> Eff es b) -> a -> Eff es c
compose g f x = Eff \es -> unEff (f x) es >>= g
{-# NOINLINE compose #-}

-- | Lift an 'IO' function to a 'Ctl' function. The function must not alter the result.
liftMap, liftMap' :: (IO (Result es a) -> IO (Result es a)) -> Ctl es a -> Ctl es a
liftMap f (Ctl m) = Ctl $ extend (liftMap' f) <$> f m
{-# INLINE liftMap #-}
liftMap' = liftMap
{-# NOINLINE liftMap' #-}

-- | Install a prompt frame.
prompt, prompt' :: (Env es' -> Env es) -> Marker e es' a -> Eff es a -> Eff es' a
prompt f !mark (Eff m) = Eff \es -> Ctl $ unCtl (m $ f es) >>= \case
  Pure a -> pure $ Pure a
  Abort mark' r -> case eqMarker mark mark' of
    Just (Refl,Refl) -> unCtl r
    Nothing   -> pure $ Abort mark' r
  Control mark' ctl cont -> case eqMarker mark mark' of
    Just (Refl,Refl) -> unCtl $ unEff (ctl (prompt' f mark . cont)) es
    Nothing   -> pure $ Control mark' ctl (prompt' f mark . cont)
{-# INLINE prompt #-}
prompt' = prompt
{-# NOINLINE prompt' #-}

-- | Unwrap the 'Ctl' monad.
runCtl :: Ctl es a -> IO a
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
newtype Eff (es :: [Effect]) (a :: Type) = Eff { unEff :: Env es -> Ctl es a }

-- | The internal representation of a handler of effect @e@. This representation is only valid within the original
-- context in which the effect was introduced.
type role InternalHandler nominal
data InternalHandler e =
    forall es a. InternalHandler
        !(Marker e es a)
        (Env es)
        (forall esSend x. e :> esSend => e (Eff esSend) x -> Eff esSend x) -- slow: `(Handler e es a)`

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
data HandleTag e (es :: [Effect]) (r :: Type) = HandleTag (Env es) !(Marker e es r)

-- | A handler of effect @e@ introduced in context @es@ over a computation returning @r@.
type Handler e es r = ∀ e' esSend a. (e' :> esSend, e :> esSend) => HandleTag e' es r -> e (Eff esSend) a -> Eff esSend a

-- | This "unsafe" @IO@ function is perfectly safe in the sense that it won't panic or otherwise cause undefined
-- behaviors; it is only unsafe when it is used to embed arbitrary @IO@ actions in any effect environment,
-- therefore breaking effect abstraction.
unsafeIO :: IO a -> Eff es a
unsafeIO m = Eff (const $ liftIO m)
{-# INLINE unsafeIO #-}

-- | Convert an effect handler into an internal representation with respect to a certain effect context and prompt
-- frame.
toInternalHandler :: ∀ e es r. Marker e es r -> Env es -> Handler e es r -> InternalHandler e
toInternalHandler mark es hdl = InternalHandler mark es (\e -> hdl (HandleTag es mark) e)

-- | Do a trivial transformation over the effect context.
alter :: (Env es' -> Env es) -> Eff es a -> Eff es' a
alter f = \(Eff m) -> Eff \es -> alterCtl f $ m $! f es
{-# INLINE alter #-}

-- | Do a trivial transformation over the effect context.
alterCtl :: (Env es' -> Env es) -> Ctl es a -> Ctl es' a
alterCtl f = \(Ctl m) -> Ctl $ m <&> \case
    Pure x -> Pure x
    Abort mark m' -> Abort mark m'
    Control mark ctl k -> Control mark ctl \b -> alter f (k b)
{-# NOINLINE alterCtl #-}

-- | General effect handling. Introduce a prompt frame, convert the supplied handler to an internal one wrt that
-- frame, and then supply the internal handler to the given function to let it add that to the effect context.
handle :: (HandlerCell e -> Env es' -> Env es) -> Handler e es' a -> Eff es a -> Eff es' a
handle f = \hdl m -> Eff \es -> do
  mark <- freshMarker
  -- let cell = toInternalHandler mark es hdl
  unEff (prompt (\es' -> f (HandlerCell $ toInternalHandler mark es' hdl) es') mark m) es
{-# INLINE handle #-}

-- | Perform an effect operation.
send :: e :> es => e (Eff es) a -> Eff es a
send e = Eff \es -> do
  case getHandlerCell (Rec.index es) of
    InternalHandler _ _ h -> unEff (h e) es
{-# INLINE send #-}

-- | Perform an operation from the handle-site.
embed :: forall e es esSend r a. e :> esSend => HandleTag e es r -> Eff es a -> Eff esSend a
embed (HandleTag es !mark) = under @e mark es
{-# INLINE embed #-}

under :: forall e es es' ans b. (e :> es) => Marker e es' ans -> Env es' -> Eff es' b -> Eff es b
under !mark evv (Eff m) = Eff \_ ->
    Ctl $ unCtl (m $! evv) <&> \case
        Pure x -> Pure x
        Abort mark' m' -> Abort mark' m'
        Control mark' ctl k -> Control mark' ctl $ resumeUnder @e mark k
{-# INLINE under #-}

resumeUnder :: forall e es es' ans a b. (e :> es) => Marker e es' ans -> (b -> Eff es' a) -> (b -> Eff es a)
resumeUnder !mark k x =
    Eff \es -> do
        case getHandlerCell (Rec.index @e es) of
            InternalHandler mark' evv' _ ->
                case eqMarker mark mark' of
                    Just (Refl,Refl) -> unEff (under @e mark evv' (k x)) es
                    Nothing -> error "unreachable"
{-# NOINLINE resumeUnder #-}

-- | Abort with a result value.
abort :: HandleTag e es r -> Eff es r -> Eff esSend a
abort (HandleTag es mark) (Eff m) = Eff \_ -> Ctl $ pure $ Abort mark (m es)
{-# INLINE abort #-}

-- | Capture and gain control of the resumption. The resumption cannot escape the scope of the controlling function.
control
  :: HandleTag e es r
  -> ((Eff esSend a -> Eff es r) -> Eff es r)
  -> Eff esSend a
control (HandleTag _ mark) f =
  Eff \_ -> Ctl $ pure $ Control mark (\cont -> f cont) id
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


instance MonadIO (Ctl es) where
  liftIO = Ctl . fmap Pure

instance MonadThrow (Ctl es) where
  throwM = Ctl . Exception.throwIO

-- | Note that although both catching and masking are possible, implementing 'Catch.generalBracket' via them will not
-- be well-behaved wrt reentry; hence 'Ctl' is not 'Catch.MonadMask'.
instance MonadCatch (Ctl es) where
  catch m h = liftMap (Exception.handle (unCtl . h)) m
