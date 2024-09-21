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
freshMarker :: Ctl eh ef (Marker e eh' ef' a)
freshMarker = liftIO $ Marker <$> fetchAddInt uniqueSource 1

-- | A @'Marker' a@ marks a prompt frame over a computation returning @a@.
type role Marker nominal nominal nominal representational
newtype Marker (e :: EffectH) (eh :: [EffectH]) (ef :: [EffectF]) (a :: Type) = Marker Int

type role FOE nominal nominal nominal
data FOE (e :: EffectF) :: EffectH

-- | Check the equality of two markers, and if so provide a proof that the type parameters are equal. This does not
-- warrant a @TestEquality@ instance because it requires decidable equality over the type parameters.
eqMarker :: Marker e eh ef a -> Marker e' eh' ef' b -> Maybe ((a,e m a) :~: (b, e' m b), eh :~: eh', ef :~: ef')
eqMarker (Marker l) (Marker r) =
  if l == r then Just (unsafeCoerce Refl, unsafeCoerce Refl, unsafeCoerce Refl) else Nothing

-- | Intermediate result of a `Ctl` computation.
type role Result nominal nominal representational
data Result eh ef (a :: Type)
  = Pure a
  -- ^ The computation returned normally.
  | ∀ (r :: Type) e eh' ef'. Abort !(Marker e eh' ef' r) (Ctl eh' ef' r)
  -- ^ The computation replaced itself with another computation.
  | ∀ (r :: Type) e eh' ef' (b :: Type). Control !(Marker e eh' ef' r) ((b -> Eff eh' ef' r) -> Eff eh' ef' r) (b -> Eff eh ef a)
  -- ^ The computation captured a resumption and gained control over it. Specifically, this uses @shift0@ semantics.

-- | Extend the captured continuation with a function, if it exists.
extend :: (Ctl eh ef a -> Ctl eh ef a) -> Result eh ef a -> Result eh ef a
extend f = \case
  Pure a                -> Pure a
  Abort mark r          -> Abort mark r
  Control mark ctl cont -> Control mark ctl ((\(Eff m) -> Eff \es -> f $ m es) . cont)

-- | The delimited control monad, with efficient support of tail-resumptive computations.
type role Ctl nominal nominal representational
newtype Ctl eh ef (a :: Type) = Ctl { unCtl :: IO (Result eh ef a) }

instance Functor (Ctl eh ef) where
  fmap = liftM

instance Applicative (Ctl eh ef) where
  pure = Ctl . pure . Pure
  (<*>) = ap

instance Monad (Ctl eh ef) where
  (Ctl x) >>= f = Ctl $ x >>= \case
    Pure a                -> unCtl (f a)
    Abort mark r          -> pure $ Abort mark r
    Control mark ctl cont -> pure $ Control mark ctl (f `compose` cont)

-- | This loopbreaker is crucial to the performance of the monad.
compose :: (b -> Ctl eh ef c) -> (a -> Eff eh ef b) -> a -> Eff eh ef c
compose g f x = Eff \es -> unEff (f x) es >>= g
{-# NOINLINE compose #-}

-- | Lift an 'IO' function to a 'Ctl' function. The function must not alter the result.
liftMap, liftMap' :: (IO (Result eh ef a) -> IO (Result eh ef a)) -> Ctl eh ef a -> Ctl eh ef a
liftMap f (Ctl m) = Ctl $ extend (liftMap' f) <$> f m
{-# INLINE liftMap #-}
liftMap' = liftMap
{-# NOINLINE liftMap' #-}

-- | Install a prompt frame.
prompt, prompt' :: (Env eh' ef' -> Env eh ef) -> Marker e eh' ef' a -> Eff eh ef a -> Eff eh' ef' a
prompt f !mark (Eff m) = Eff \es -> Ctl $ unCtl (m $ f es) >>= \case
  Pure a -> pure $ Pure a
  Abort mark' r -> case eqMarker mark mark' of
    Just (Refl,Refl,Refl) -> unCtl r
    Nothing   -> pure $ Abort mark' r
  Control mark' ctl cont -> case eqMarker mark mark' of
    Just (Refl,Refl,Refl) -> unCtl $ unEff (ctl (prompt' f mark . cont)) es
    Nothing   -> pure $ Control mark' ctl (prompt' f mark . cont)
{-# INLINE prompt #-}
prompt' = prompt
{-# NOINLINE prompt' #-}

-- | Unwrap the 'Ctl' monad.
runCtl :: Ctl eh ef a -> IO a
runCtl (Ctl m) = m >>= \case
  Pure a     -> pure a
  Abort {}   -> error "Sp.Ctl: Unhandled abort operation. Forgot to pair it with a prompt?"
  Control {} -> error "Sp.Ctl: Unhandled control operation. Forgot to pair it with a prompt?"


-- | The kind of higher-order effects, parameterized by (1) the monad in which it was performed, and (2) the result
-- type.
type EffectH = (Type -> Type) -> Type -> Type
type EffectF = Type -> Type

-- | The concrete representation of an effect context: a record of internal handler representations.
data Env eh ef = Env { getEnvH :: !(Rec (InternalElaborator eh ef) eh), getEnvF :: !(Rec InternalHandler ef) }

-- | The effect monad; it is parameterized by the /effect context/, i.e. a row of effects available. This monad is
-- implemented with evidence passing and a delimited control monad with support of efficient tail-resumptive
-- (non-capturing) computations and @IO@ embedding.
type role Eff nominal nominal representational
newtype Eff (eh :: [EffectH]) (ef :: [EffectF]) (a :: Type) = Eff { unEff :: Env eh ef -> Ctl eh ef a }

type role InternalHandler nominal
data InternalHandler e =
    forall eh ef a. InternalHandler
        !(Marker (FOE e) eh ef a)
        (Env eh ef)
        (forall ehSend efSend x. e :> efSend => e x -> Eff ehSend efSend x) -- slow: `(Handler e eh ef a)`

type role InternalElaborator nominal nominal nominal
data InternalElaborator eh' ef' e =
    forall eh ef a. InternalElaborator
        !(Marker e eh ef a)
        (Env eh ef)
        (forall ehSend efSend x. e :> ehSend => e (Eff eh' ef') x -> Eff ehSend efSend x) -- slow: `(Elaborator e eh ef a)`

data LiftIns (e :: EffectF) (m :: Type -> Type) a = LiftIns { unLiftIns :: e a }

instance Functor (Eff eh ef) where
  fmap = liftM
  {-# INLINE fmap #-}

instance Applicative (Eff eh ef) where
  pure x = Eff \_ -> pure x
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Monad (Eff eh ef) where
  Eff m >>= f = Eff \es -> m es >>= \x -> unEff (f x) es
  {-# INLINE (>>=) #-}

-- | The tag associated to a handler that was /introduced/ in context @es@ over an computation with
-- /eventual result type/ @r@. Value of this type enables delimited control and scoped effects.
data InterpretTag e (eh :: [EffectH]) (ef :: [EffectF]) (r :: Type) = InterpretTag (Env eh ef) !(Marker e eh ef r)

-- | A handler of effect @e@ introduced in context @es@ over a computation returning @r@.
type Handler e eh ef r = ∀ e' ehSend efSend a. e' :> efSend => InterpretTag (FOE e') eh ef r -> e a -> Eff ehSend efSend a

type Elaborator e eh ef r = ∀ e' ehSend efSend a. e' :> ehSend => InterpretTag e' eh ef r -> e (Eff eh ef) a -> Eff ehSend efSend a

-- | This "unsafe" @IO@ function is perfectly safe in the sense that it won't panic or otherwise cause undefined
-- behaviors; it is only unsafe when it is used to embed arbitrary @IO@ actions in any effect environment,
-- therefore breaking effect abstraction.
unsafeIO :: IO a -> Eff eh ef a
unsafeIO m = Eff (const $ liftIO m)
{-# INLINE unsafeIO #-}

-- | Convert an effect handler into an internal representation with respect to a certain effect context and prompt
-- frame.
toInternalElaborator :: ∀ e eh ef r. Marker e eh ef r -> Env eh ef -> Elaborator e eh ef r -> InternalElaborator eh ef e
toInternalElaborator mark es hdl = InternalElaborator mark es (\e -> hdl (InterpretTag es mark) e)

-- | Convert an effect handler into an internal representation with respect to a certain effect context and prompt
-- frame.
toInternalHandler :: ∀ e eh ef r. Marker (FOE e) eh ef r -> Env eh ef -> Handler e eh ef r -> InternalHandler e
toInternalHandler mark es hdl = InternalHandler mark es (\e -> hdl (InterpretTag es mark) e)

-- | Do a trivial transformation over the effect context.
alter :: (Env eh' ef' -> Env eh ef) -> Eff eh ef a -> Eff eh' ef' a
alter f = \(Eff m) -> Eff \es -> alterCtl f $ m $! f es
{-# INLINE alter #-}

-- | Do a trivial transformation over the effect context.
alterCtl :: (Env eh' ef' -> Env eh ef) -> Ctl eh ef a -> Ctl eh' ef' a
alterCtl f = \(Ctl m) -> Ctl $ m <&> \case
    Pure x -> Pure x
    Abort mark m' -> Abort mark m'
    Control mark ctl k -> Control mark ctl \b -> alter f (k b)
{-# NOINLINE alterCtl #-}

alterRec ::
    HFunctors eh =>
    (Rec (InternalElaborator eh' ef') eh' -> Rec (InternalElaborator eh' ef') eh) ->
    (Rec InternalHandler ef' -> Rec InternalHandler ef) ->
    Eff eh ef a -> Eff eh' ef' a
alterRec mapH mapF = alter $ alterEnvRec mapH mapF

alterEnvRec ::
    HFunctors eh =>
    (Rec (InternalElaborator eh' ef') eh' -> Rec (InternalElaborator eh' ef') eh) ->
    (Rec InternalHandler ef' -> Rec InternalHandler ef) ->
    Env eh' ef' -> Env eh ef
alterEnvRec mapH mapF = hfmapEnv mapH mapF $ alterRec mapH mapF

hfmapEnv ::
    HFunctors eh =>
    (Rec (InternalElaborator eh' ef') eh' -> Rec (InternalElaborator eh' ef') eh) ->
    (Rec InternalHandler ef' -> Rec InternalHandler ef) ->
    (forall x. Eff eh ef x -> Eff eh' ef' x) ->
    Env eh' ef' -> Env eh ef
hfmapEnv mapH mapF f =
    alterEnv
        (
              (Rec.map @HFunctor \(InternalElaborator mark es elb) ->
                InternalElaborator mark es $ elb . hfmap f)
            . mapH
        )
        mapF

alterEnv ::
    (Rec (InternalElaborator eh' ef') eh' -> Rec (InternalElaborator eh ef) eh) ->
    (Rec InternalHandler ef' -> Rec InternalHandler ef) ->
    Env eh' ef' -> Env eh ef
alterEnv mapH mapF = \(Env eh ef) -> Env (mapH eh) (mapF ef)
{-# INLINE alterEnv #-}

class HFunctor h where
    hfmap :: (forall x. f x -> g x) -> h f a -> h g a

type HFunctors = Rec.Forall HFunctor

-- | General effect handling. Introduce a prompt frame, convert the supplied handler to an internal one wrt that
-- frame, and then supply the internal handler to the given function to let it add that to the effect context.
handle :: (InternalHandler e -> Env eh' ef' -> Env eh ef) -> Handler e eh' ef' a -> Eff eh ef a -> Eff eh' ef' a
handle f = \hdl m -> Eff \es -> do
  mark <- freshMarker
  unEff (prompt (\es' -> f (toInternalHandler mark es' hdl) es') mark m) es
{-# INLINE handle #-}

-- | General effect handling. Introduce a prompt frame, convert the supplied handler to an internal one wrt that
-- frame, and then supply the internal handler to the given function to let it add that to the effect context.
handleH :: (InternalElaborator eh' ef' e -> Env eh' ef' -> Env eh ef) -> Elaborator e eh' ef' a -> Eff eh ef a -> Eff eh' ef' a
handleH f = \elb m -> Eff \es -> do
  mark <- freshMarker
  unEff (prompt (\es' -> f (toInternalElaborator mark es' elb) es') mark m) es
{-# INLINE handleH #-}

-- | Perform an effect operation.
send :: e :> ef => e a -> Eff eh ef a
send e = Eff \es@(Env _ ef) -> do
  case Rec.index ef of
    InternalHandler _ _ h -> unEff (h e) es
{-# INLINE send #-}

-- | Perform an effect operation.
sendH :: e :> eh => e (Eff eh ef) a -> Eff eh ef a
sendH e = Eff \es@(Env eh _) -> do
  case Rec.index eh of
    InternalElaborator _ _ h -> unEff (h e) es
{-# INLINE sendH #-}

-- | Perform an operation from the handle-site.
embed :: forall e eh ef ehSend efSend r a. e :> efSend => InterpretTag (FOE e) eh ef r -> Eff eh ef a -> Eff ehSend efSend a
embed (InterpretTag es !mark) = under @e mark es
{-# INLINE embed #-}

under :: forall e eh ef eh' ef' ans b. (e :> ef) => Marker (FOE e) eh' ef' ans -> Env eh' ef' -> Eff eh' ef' b -> Eff eh ef b
under !mark evv (Eff m) = Eff \_ ->
    Ctl $ unCtl (m $! evv) <&> \case
        Pure x -> Pure x
        Abort mark' m' -> Abort mark' m'
        Control mark' ctl k -> Control mark' ctl $ resumeUnder @e mark k
{-# INLINE under #-}

resumeUnder :: forall e eh ef eh' ef' ans a b. (e :> ef) => Marker (FOE e) eh' ef' ans -> (b -> Eff eh' ef' a) -> (b -> Eff eh ef a)
resumeUnder !mark k x =
    Eff \es@(Env _ ef) -> do
        case Rec.index @e ef of
            InternalHandler mark' es' _ ->
                case eqMarker mark mark' of
                    Just (Refl,Refl,Refl) -> unEff (under @e mark es' (k x)) es
                    Nothing -> error "unreachable"
{-# NOINLINE resumeUnder #-}

-- | Perform an operation from the handle-site.
embedH :: forall e eh ef ehSend efSend r a. e :> ehSend => InterpretTag e eh ef r -> Eff eh ef a -> Eff ehSend efSend a
embedH (InterpretTag es !mark) = underH @e mark es
{-# INLINE embedH #-}

underH :: forall e eh ef eh' ef' ans b. (e :> eh) => Marker e eh' ef' ans -> Env eh' ef' -> Eff eh' ef' b -> Eff eh ef b
underH !mark evv (Eff m) = Eff \_ ->
    Ctl $ unCtl (m $! evv) <&> \case
        Pure x -> Pure x
        Abort mark' m' -> Abort mark' m'
        Control mark' ctl k -> Control mark' ctl $ resumeUnderH @e mark k
{-# INLINE underH #-}

resumeUnderH :: forall e eh ef eh' ef' ans a b. (e :> eh) => Marker e eh' ef' ans -> (b -> Eff eh' ef' a) -> (b -> Eff eh ef a)
resumeUnderH !mark k x =
    Eff \es@(Env eh _) -> do
        case Rec.index @e eh of
            InternalElaborator mark' es' _ ->
                case eqMarker mark mark' of
                    Just (Refl,Refl,Refl) -> unEff (underH @e mark es' (k x)) es
                    Nothing -> error "unreachable"
{-# NOINLINE resumeUnderH #-}

-- | Abort with a result value.
abort :: InterpretTag e eh ef r -> Eff eh ef r -> Eff ehSend efSend a
abort (InterpretTag es mark) (Eff m) = Eff \_ -> Ctl $ pure $ Abort mark (m es)
{-# INLINE abort #-}

-- | Capture and gain control of the resumption. The resumption cannot escape the scope of the controlling function.
control
  :: InterpretTag e eh ef r
  -> ((Eff ehSend efSend a -> Eff eh ef r) -> Eff eh ef r)
  -> Eff ehSend efSend a
control (InterpretTag _ mark) f =
  Eff \_ -> Ctl $ pure $ Control mark (\cont -> f cont) id
{-# INLINE control #-}

-- | Unwrap the 'Eff' monad.
runEff :: Eff '[] '[] a -> a
runEff (Eff m) = unsafePerformIO (runCtl $ m $ Env Rec.empty Rec.empty)
{-# INLINE runEff #-}

-- | Ability to embed 'IO' side effects.
data IOE :: EffectF

instance IOE :> ef => MonadIO (Eff eh ef) where
  liftIO = unsafeIO
  {-# INLINE liftIO #-}

instance MonadThrow (Eff eh ef) where
  throwM x = Eff \_ -> Catch.throwM x
  {-# INLINE throwM #-}

instance IOE :> es => MonadCatch (Eff eh ef) where
  catch (Eff m) h = Eff \es -> Catch.catch (m es) \ex -> unEff (h ex) es
  {-# INLINE catch #-}

-- | Unwrap an 'Eff' monad with 'IO' computations.
runIOE :: Eff '[] '[IOE] a -> IO a
runIOE m = runCtl $ unEff m $ Env Rec.empty (Rec.pad Rec.empty)
{-# INLINE runIOE #-}

instance MonadIO (Ctl eh ef) where
  liftIO = Ctl . fmap Pure

instance MonadThrow (Ctl eh ef) where
  throwM = Ctl . Exception.throwIO

-- | Note that although both catching and masking are possible, implementing 'Catch.generalBracket' via them will not
-- be well-behaved wrt reentry; hence 'Ctl' is not 'Catch.MonadMask'.
instance MonadCatch (Ctl eh ef) where
  catch m h = liftMap (Exception.handle (unCtl . h)) m
