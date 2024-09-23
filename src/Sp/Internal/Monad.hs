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

import           Control.Monad          (ap, liftM, (<=<), (>=>))
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Kind              (Type)
import qualified Sp.Internal.Env        as Rec
import           Sp.Internal.Env        (Rec, (:>))
import           System.IO.Unsafe       (unsafePerformIO)
import           Data.Primitive.PrimVar (PrimVar, fetchAddInt, newPrimVar)
import           Data.Type.Equality     (type (:~:) (Refl))
import           GHC.Exts               (RealWorld)
import           Unsafe.Coerce          (unsafeCoerce)
import Data.Functor ((<&>))
import Sp.Internal.FTCQueue

-- | The source from which we construct unique 'Marker's.
uniqueSource :: PrimVar RealWorld Int
uniqueSource = unsafePerformIO (newPrimVar 0)
{-# NOINLINE uniqueSource #-}

-- | Create a fresh 'Marker'.
freshMarker :: Eff es (Marker e es' a)
freshMarker = Eff \_ -> Ctl $ fmap Pure $ Marker <$> fetchAddInt uniqueSource 1

-- | A @'Marker' a@ marks a prompt frame over a computation returning @a@.
type role Marker nominal nominal representational
newtype Marker (e :: Effect) (es :: [Effect]) (a :: Type) = Marker Int

-- | Check the equality of two markers, and if so provide a proof that the type parameters are equal. This does not
-- warrant a @TestEquality@ instance because it requires decidable equality over the type parameters.
eqMarker :: Marker e es a -> Marker e' es' b -> Maybe ((a,e m a) :~: (b, e' m b), es :~: es')
eqMarker (Marker l) (Marker r) =
  if l == r then Just (unsafeCoerce Refl, unsafeCoerce Refl) else Nothing

-- | Intermediate result of a `Ctl` computation.
type role Result nominal nominal
data Result es (a :: Type)
  = Pure a
  -- ^ The computation returned normally.
  | ∀ (r :: Type) e es'. Abort !(Marker e es' r) (Ctl es' r)
  -- ^ The computation replaced itself with another computation.
  | ∀ (r :: Type) e es' (b :: Type). Control !(Marker e es' r) ((b -> Eff es' r) -> Eff es' r) (FTCQueue (Eff es) b a)
  -- ^ The computation captured a resumption and gained control over it. Specifically, this uses @shift0@ semantics.

-- | The delimited control monad, with efficient support of tail-resumptive computations.
type role Ctl nominal nominal
newtype Ctl es (a :: Type) = Ctl { unCtl :: IO (Result es a) }

-- | Install a prompt frame.
prompt, prompt' :: (Env es' -> Env es) -> Marker e es' a -> Eff es a -> Eff es' a
prompt f !mark (Eff m) = Eff \es -> Ctl $ unCtl (m $ f es) >>= \case
  Pure a -> pure $ Pure a
  Abort mark' r -> case eqMarker mark mark' of
    Just (Refl,Refl) -> unCtl r
    Nothing   -> pure $ Abort mark' r
  Control mark' ctl cont -> case eqMarker mark mark' of
    Just (Refl,Refl) -> unCtl $ unEff (ctl (prompt' f mark . qApp cont)) es
    Nothing   -> pure $ Control mark' ctl (tsingleton $ prompt' f mark . qApp cont)
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
type role Eff nominal nominal
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
  pure x = Eff \_ -> Ctl $ pure $ Pure $ x
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Monad (Eff es) where
  Eff m >>= f = Eff \es -> Ctl $ unCtl (m es) >>= \case
    Pure a                -> unCtl (unEff (f a) es)
    Abort mark r          -> pure $ Abort mark r
    Control mark ctl cont -> pure $ Control mark ctl (cont |> f)
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
unsafeIO m = Eff (const $ Ctl $ fmap Pure $ m)
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
    Control mark ctl k -> Control mark ctl (tsingleton $ alterFQ f k)
{-# INLINE alterCtl #-}

alterFQ :: (Env es' -> Env es) -> FTCQueue (Eff es) a b -> a -> Eff es' b
alterFQ f q = case tviewl q of
    TOne k -> \x -> alter f (k x)
    k :| kk -> \x -> alter f (k x) >>= alterFQ f kk
{-# NOINLINE alterFQ #-}

-- | General effect handling. Introduce a prompt frame, convert the supplied handler to an internal one wrt that
-- frame, and then supply the internal handler to the given function to let it add that to the effect context.
handle :: (HandlerCell e -> Env es' -> Env es) -> Handler e es' a -> Eff es a -> Eff es' a
handle f = \hdl m -> do
  mark <- freshMarker
  -- let cell = toInternalHandler mark es hdl
  prompt (\es' -> f (HandlerCell $ toInternalHandler mark es' hdl) es') mark m
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

qApp :: FTCQueue (Eff es) a b -> a -> Eff es b
qApp q' x = Eff \es -> case tviewl q' of
  TOne k  -> unEff (k x) es
  k :| t -> Ctl $ unCtl (unEff (k x) es) >>= \case
    Pure r -> unCtl $ unEff (qApp t r) es
    Abort mark m -> pure $ Abort mark m
    Control mark ctl k' -> pure $ Control mark ctl $ k' >< t

under :: forall e es es' ans b. (e :> es) => Marker e es' ans -> Env es' -> Eff es' b -> Eff es b
under !mark evv (Eff m) = Eff \_ ->
    Ctl $ unCtl (m $! evv) <&> \case
        Pure x -> Pure x
        Abort mark' m' -> Abort mark' m'
        Control mark' ctl k -> Control mark' ctl $ tsingleton $ resumeUnder @e mark $ qApp k
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
  Eff \_ -> Ctl $ pure $ Control mark (\cont -> f cont) (tsingleton id)
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

-- | Unwrap an 'Eff' monad with 'IO' computations.
runIOE :: Eff '[IOE] a -> IO a
runIOE m = runCtl $ unEff m (Rec.pad Rec.empty)
{-# INLINE runIOE #-}
