{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE QuantifiedConstraints #-}

{- |
 Copyright: (c) 2022 Xy Ren
 License: BSD3
 Maintainer: xy.r@outlook.com
 Stability: experimental
 Portability: non-portable (GHC only)

 The effect monad, along with handling combinators that enable delimited control and higher-order effects.
-}
module Sp.Internal.Monad where

import Control.Monad (ap, liftM, (<=<))
import Data.Functor.Identity (Identity, runIdentity)
import Data.Kind (Type)
import Data.Primitive.PrimVar (PrimVar, fetchAddInt, newPrimVar)
import Data.Type.Equality ((:~:), type (:~:) (Refl))
import GHC.Exts (RealWorld)
import Sp.Internal.Env (Rec, (:>))
import Sp.Internal.Env qualified as Rec
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)
import Data.Function ((&))
import Data.Functor ((<&>))
import Control.Monad.IO.Class (MonadIO, liftIO)

{- | The kind of first-order effects, parameterized by the result type.
-}
type Effect = Type -> Type

-- | A @'Marker' a@ marks a prompt frame over a computation returning @a@.
type role Marker nominal nominal representational

newtype Marker (e :: Effect) (es :: [Effect]) (a :: Type) = Marker Int

-- | The source from which we construct unique 'Marker's.
uniqueSource :: PrimVar RealWorld Int
uniqueSource = unsafePerformIO (newPrimVar 0)
{-# NOINLINE uniqueSource #-}

-- | Create a fresh 'Marker'.
freshMarker :: forall e es m a. (Marker e es a -> Eff m es a) -> Eff m es a
freshMarker f =
    let mark = unsafePerformIO $ fetchAddInt uniqueSource 1
     in seq mark (f $ Marker mark)
{-# NOINLINE freshMarker #-}

{- | Check the equality of two markers, and if so provide a proof that the type parameters are equal. This does not
 warrant a @TestEquality@ instance because it requires decidable equality over the type parameters.
-}
eqMarker :: Marker e es a -> Marker e' es' b -> Maybe ((e a, a) :~: (e' b, b), es :~: es')
eqMarker (Marker l) (Marker r) =
    if l == r then Just (unsafeCoerce Refl, unsafeCoerce Refl) else Nothing

-- | The delimited control monad, with efficient support of tail-resumptive computations.
data Ctl m es (a :: Type)
    = Pure a
    | forall (r :: Type) (b :: Type) e es'.
        Control !(Marker e es' r) ((b -> Eff m es' r) -> Eff m es' r) (b -> Eff m es a)

{- | The effect monad; it is parameterized by the /effect context/, i.e. a row of effects available. This monad is
 implemented with evidence passing and a delimited control monad with support of efficient tail-resumptive
 (non-capturing) computations and @IO@ embedding.
-}
newtype Eff m es a = Eff {unEff :: Env m es -> m (Ctl m es a)}

-- | Extend the captured continuation with a function, if it exists.
extend :: (Eff m es a -> Eff m es a) -> Ctl m es a -> Ctl m es a
extend f = \case
    Pure a -> Pure a
    Control mark ctl cont -> Control mark ctl (f . cont)

instance Monad m => Functor (Eff m es) where
    fmap = liftM

instance Monad m => Applicative (Eff m es) where
    pure x = Eff \_ -> pure $ Pure x
    (<*>) = ap

instance Monad m => Monad (Eff m es) where
    Eff m >>= f =
        Eff \evv -> do
            (m $! evv) >>= \case
                Pure x -> unEff (f x) $! evv
                Control mark ctl cont -> pure $ Control mark ctl (f `compose` cont)

-- | This loopbreaker is crucial to the performance of the monad.
compose :: Monad m => (b -> Eff m es c) -> (a -> Eff m es b) -> a -> Eff m es c
compose = (<=<)
{-# NOINLINE compose #-}

-- | The concrete representation of an effect context: a record of internal handler representations.
type Env m = Rec (Evidence m) :: [Effect] -> Type

data Evidence m (e :: Effect)
    = forall es ans. Evidence !(Marker e es ans) !(Env m es) (Handler m e es ans)

type Handler m e es ans = forall e' esSend x. e' :> esSend => HandleTag m e' es ans -> e x -> Eff m esSend x
data HandleTag m e es ans = HandleTag !(Marker e es ans) (Env m es)

-- use a prompt with a unique marker (and handle yields to it)
prompt :: Monad m => (Env m es' -> Env m es) -> Marker e es' ans ->  Eff m es ans -> Eff m es' ans
prompt f !mark (Eff m) = Eff \evv ->
    (m $! f evv) >>= \case
        Pure x -> pure $ Pure x -- add handler to the context
        Control mark' ctl k ->
            let k' = prompt f mark . k -- extend the continuation with our own prompt
             in case eqMarker mark mark' of
                    Nothing -> pure $ Control mark' ctl k' -- keep yielding (but with the extended continuation)
                    Just (Refl, Refl) -> unEff (ctl k') $! evv -- found our prompt, invoke `op` (under the context `evv`).
                    -- Note: `Refl` proves `a ~ ans` and `es ~ es'` (the existential `ans,es'` in Control)
{-# INLINE prompt #-}

handle :: Monad m => (Evidence m e -> Env m es' -> Env m es) -> Handler m e es' ans -> Eff m es ans -> Eff m es' ans
handle f h action =
    freshMarker \mark ->
        prompt (\evv -> f (Evidence mark evv h) evv) mark action
{-# INLINE handle #-}

rehandle :: (e :> es, Monad m) => (Env m es' -> Env m es) -> Handler m e es' ans -> Eff m es ans -> Eff m es' ans
rehandle f = handle \_ -> f
{-# INLINE rehandle #-}

-- | Do a trivial transformation over the effect context.
alter :: Monad m => (Env m es' -> Env m es) -> Eff m es a -> Eff m es' a
alter f = \(Eff m) -> Eff \evv -> alterCtl f <$> (m $! f evv)
{-# INLINE alter #-}

-- | Do a trivial transformation over the effect context.
alterCtl :: Monad m => (Env m es' -> Env m es) -> Ctl m es a -> Ctl m es' a
alterCtl f = \case
    Pure x -> Pure x
    Control mark ctl k -> Control mark ctl \b -> alter f (k b)
{-# INLINE alterCtl #-}

under :: (e :> es, Monad m) => Marker e es' ans -> Env m es' -> Eff m es' b -> Eff m es b
under !mark evv (Eff m) = Eff \_ ->
    (m $! evv) <&> \case
        Pure x -> Pure x
        Control mark' ctl k -> Control mark' ctl $ resumeUnder mark k

resumeUnder :: forall e es es' ans m a b. (e :> es, Monad m) => Marker e es' ans -> (b -> Eff m es' a) -> (b -> Eff m es a)
resumeUnder !mark k x =
    withEvidence @e \(Evidence mark' evv' _) ->
        case eqMarker mark mark' of
            Just (Refl,Refl) -> under mark evv' (k x)
            Nothing -> error "unreachable"

send :: forall e es m a. (e :> es, Monad m) => e a -> Eff m es a
send e = withEvidence \(Evidence marker evv' h) -> h (HandleTag marker evv') e
{-# INLINE send #-}

embed :: (Monad m, e' :> esSend) => HandleTag m e' es ans -> Eff m es a -> Eff m esSend a
embed (HandleTag !mark evv) =  under mark evv
{-# INLINE embed #-}

control :: Monad m => HandleTag m e' es ans -> ((a -> Eff m es ans) -> Eff m  es ans) -> Eff m esSend a
control (HandleTag !mark _) f = Eff \_ -> pure $ Control mark f pure
{-# INLINE control #-}

abort :: Monad m => HandleTag m e' es ans -> Eff m es ans -> Eff m esSend b
abort tag m = control tag \_ -> m
{-# INLINE abort #-}

withEvidence :: (e :> es, Monad m) => (Evidence m e -> Eff m es a) -> Eff m es a
withEvidence f =  Eff \evv -> unEff (Rec.index evv & \ev -> f ev) $! evv
{-# INLINE withEvidence #-}

instance MonadIO m => MonadIO (Eff m es) where
    liftIO m = Eff \_ -> Pure <$> liftIO m

runEff :: Functor m => Eff m '[] a -> m a
runEff (Eff m) = (m $! Rec.empty) <&> \case
    Pure x -> x
    Control _ _ _ -> error "unreachable"
{-# INLINE runEff #-}

runPure :: Eff Identity '[] a -> a
runPure = runIdentity . runEff
{-# INLINE runPure #-}

runPure' :: Eff IO '[] a -> a
runPure' = unsafePerformIO . runEff
{-# INLINE runPure' #-}
