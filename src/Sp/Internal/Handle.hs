{-# LANGUAGE AllowAmbiguousTypes #-}
-- |
-- Copyright: (c) 2022 Xy Ren
-- License: BSD3
-- Maintainer: xy.r@outlook.com
-- Stability: experimental
-- Portability: non-portable (GHC only)
--
-- Functions for effect handling, as well as effect stack manipulating.
module Sp.Internal.Handle where

import qualified Sp.Internal.Env   as Rec
import           Sp.Internal.Env   (KnownList, Suffix, type (++), (:>))
import           Sp.Internal.Monad

--------------------------------------------------------------------------------
-- Interpret -------------------------------------------------------------------
--------------------------------------------------------------------------------

-- The family of /interpreting/ functions. Eliminate an effect from the top of the stack via a handler, and
-- optionally add some other effects (@es'@) that could be used in the handler. Adding these effects instead of
-- requiring them on the client side achieves effect encapsulation.

type Interpret ef' = ∀ e ef eh a. Handler e eh (ef' ++ ef) a -> Eff '[] (e : ef) a -> Eff eh (ef' ++ ef) a
type InterpretRec ef' = ∀ e ef eh a. HFunctors eh => (forall x. Handler e eh (ef' ++ ef) x) -> Eff eh (e : ef) a -> Eff eh (ef' ++ ef) a
type InterpretRecH eh' = ∀ e eh ef a. (HFunctors eh, HFunctor e) => (forall x. Elaborator e (eh' ++ eh) ef x) -> Eff (e ': eh) ef a -> Eff (eh' ++ eh) ef a

-- | Interpret and add extra effects based on type inference. If this does not work consider using the more concrete
-- functions below.
interpret,interpret' :: Suffix ef ef' => Handler e eh ef' a -> Eff '[] (e ': ef) a -> Eff eh ef' a
interpret = handle \ih -> alterEnvF (Rec.cons ih . Rec.suffix)
{-# INLINE interpret #-}
interpret' = interpret
{-# NOINLINE interpret' #-}

-- | Interpret and don't add extra effects.
interpret0 :: Interpret '[]
interpret0 = interpret
{-# INLINE interpret0 #-}

-- | Interpret and add a list of extra effects specified explicitly via @TypeApplications@.
interpretN, interpretN' :: ∀ es'. KnownList es' => Interpret es'
interpretN = handle \ih -> alterEnvF (Rec.cons ih . Rec.drop @es')
{-# INLINE interpretN #-}
interpretN' = interpretN @es'
{-# NOINLINE interpretN' #-}

-- | Interpret and add extra effects based on type inference. If this does not work consider using the more concrete
-- functions below.
interpretRec,interpretRec' :: (Suffix ef ef', HFunctors eh) => (forall x. Handler e eh ef' x) -> Eff eh (e : ef) a -> Eff eh ef' a
interpretRec hdl = handle (\ih -> hfmapEnv id (Rec.cons ih . Rec.suffix) (interpretRec' hdl)) hdl
{-# INLINE interpretRec #-}
interpretRec' = interpretRec
{-# NOINLINE interpretRec' #-}

-- | Interpret and don't add extra effects.
interpretRec0 :: InterpretRec '[]
interpretRec0 = interpretRec
{-# INLINE interpretRec0 #-}

-- | Interpret and add a list of extra effects specified explicitly via @TypeApplications@.
interpretRecN, interpretRecN' :: ∀ es'. KnownList es' => InterpretRec es'
interpretRecN hdl = handle (\ih -> hfmapEnv id (Rec.cons ih . Rec.drop @es') (interpretRecN' @es' hdl)) hdl
{-# INLINE interpretRecN #-}
interpretRecN' = interpretRecN @es'
{-# NOINLINE interpretRecN' #-}

-- | Interpret and add extra effects based on type inference. If this does not work consider using the more concrete
-- functions below.
interpretRecH,interpretRecH' :: (Suffix eh eh', HFunctors eh, HFunctor e) => (forall x. Elaborator e eh' ef x) -> Eff (e ': eh) ef a -> Eff eh' ef a
interpretRecH hdl = elaborate (\ie -> hfmapEnv (Rec.cons ie . Rec.suffix) id (interpretRecH' hdl)) hdl
{-# INLINE interpretRecH #-}
interpretRecH' = interpretRecH
{-# NOINLINE interpretRecH' #-}

-- | Interpret and don't add extra effects.
interpretRec0H :: InterpretRecH '[]
interpretRec0H = interpretRecH
{-# INLINE interpretRec0H #-}

-- | Interpret and add a list of extra effects specified explicitly via @TypeApplications@.
interpretRecNH, interpretRecNH' :: ∀ es'. KnownList es' => InterpretRec es'
interpretRecNH elb = handle (\ie -> hfmapEnv id (Rec.cons ie . Rec.drop @es') (interpretRecN' @es' elb)) elb
{-# INLINE interpretRecNH #-}
interpretRecNH' = interpretRecN @es'
{-# NOINLINE interpretRecNH' #-}

--------------------------------------------------------------------------------
-- Interpose -------------------------------------------------------------------
--------------------------------------------------------------------------------

-- The family of /interposing/ functions. Modify the implementation of an effect in the stack via a new handler, and
-- optionally add some other effects (@es'@) that could be used in said handler.

type Interpose ef' = ∀ e ef eh a. e :> ef => Handler e eh (ef' ++ ef) a -> Eff '[] ef a -> Eff eh (ef' ++ ef) a
type InterposeRec ef' = ∀ e ef eh a. (e :> ef, HFunctors eh) => (forall x. Handler e eh (ef' ++ ef) x) -> Eff eh ef a -> Eff eh (ef' ++ ef) a
type InterposeRecH eh' = ∀ e eh ef a. (e :> eh, HFunctors eh) => (forall x. Elaborator e (eh' ++ eh) ef x) -> Eff eh ef a -> Eff (eh' ++ eh) ef a

-- | Interpose and add extra effects based on type inference. If this does not work consider using the more concrete
-- functions below.
interpose,interpose' :: (e :> ef, Suffix ef ef') => Handler e eh ef' a -> Eff '[] ef a -> Eff eh ef' a
interpose = handle \ie -> alterEnvF $ Rec.update ie . Rec.suffix
{-# INLINE interpose #-}
interpose' = interpose
{-# NOINLINE interpose' #-}

-- | Interpose and don't add extra effects.
interpose0 :: Interpose '[]
interpose0 = interpose
{-# INLINE interpose0 #-}

-- | Interpose and add a list of extra effects specified explicitly via @TypeApplications@.
interposeN,interposeN' :: ∀ es'. KnownList es' => Interpose es'
interposeN = handle \ie -> alterEnvF $ Rec.update ie . Rec.drop @es'
{-# INLINE interposeN #-}
interposeN' = interposeN @es'
{-# NOINLINE interposeN' #-}

-- | Interpose and add extra effects based on type inference. If this does not work consider using the more concrete
-- functions below.
interposeRec,interposeRec' :: (e :> ef, Suffix ef ef', HFunctors eh) => (forall x. Handler e eh ef' x) -> Eff eh ef a -> Eff eh ef' a
interposeRec hdl = handle (\ie -> hfmapEnv id (Rec.update ie . Rec.suffix) (interposeRec' hdl)) hdl
{-# INLINE interposeRec #-}
interposeRec' = interposeRec
{-# NOINLINE interposeRec' #-}

-- | Interpose and don't add extra effects.
interposeRec0 :: InterposeRec '[]
interposeRec0 = interposeRec
{-# INLINE interposeRec0 #-}

-- | Interpose and add a list of extra effects specified explicitly via @TypeApplications@.
interposeRecN,interposeRecN' :: ∀ es'. KnownList es' => InterposeRec es'
interposeRecN hdl = handle (\ie -> hfmapEnv id (Rec.update ie . Rec.drop @es') (interposeRecN' @es' hdl)) hdl
{-# INLINE interposeRecN #-}
interposeRecN' = interposeRecN @es'
{-# NOINLINE interposeRecN' #-}

-- | Interpose and add extra effects based on type inference. If this does not work consider using the more concrete
-- functions below.
interposeRecH,interposeRecH' :: (e :> eh, Suffix eh eh', HFunctors eh) => (forall x. Elaborator e eh' ef x) -> Eff eh ef a -> Eff eh' ef a
interposeRecH elb = elaborate (\ie -> hfmapEnv (Rec.update ie . Rec.suffix) id (interposeRecH' elb)) elb
{-# INLINE interposeRecH #-}
interposeRecH' = interposeRecH
{-# NOINLINE interposeRecH' #-}

-- | Interpose and don't add extra effects.
interposeRec0H :: InterposeRecH '[]
interposeRec0H = interposeRecH
{-# INLINE interposeRec0H #-}

-- | Interpose and add a list of extra effects specified explicitly via @TypeApplications@.
interposeRecNH,interposeRecNH' :: ∀ es'. KnownList es' => InterposeRecH es'
interposeRecNH elb = elaborate (\ie -> hfmapEnv (Rec.update ie . Rec.drop @es') id (interposeRecNH' @es' elb)) elb
{-# INLINE interposeRecNH #-}
interposeRecNH' = interposeRecNH @es'
{-# NOINLINE interposeRecNH' #-}

--------------------------------------------------------------------------------
-- Lift ------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | The family of /lifting/ functions. They trivially lift a computation over some effects into a larger effect
-- context. It can be also seen as masking a set of effects from the inner computation.
type Lift ef' = ∀ eh ef a. HFunctors eh => Eff eh ef a -> Eff eh (ef' ++ ef) a

-- | Lift over some effects based on type inference. If this does not work consider using the more concrete functions
-- below.
lift :: (Suffix ef ef', HFunctors eh) => Eff eh ef a -> Eff eh ef' a
lift = alterRec id Rec.suffix
{-# INLINE lift #-}

-- | Lift over 1 effect.
lift1 :: Lift '[e']
lift1 = lift
{-# INLINE lift1 #-}

-- | Lift over 2 effects.
lift2 :: Lift '[e', e'']
lift2 = lift
{-# INLINE lift2 #-}

-- | Lift over 3 effects.
lift3 :: Lift '[e', e'', e''']
lift3 = lift
{-# INLINE lift3 #-}

-- | Lift pver a list of effects supplied explicitly via @TypeApplications@.
liftN :: ∀ ef'. KnownList ef' => Lift ef'
liftN = alterRec id (Rec.drop @ef')
{-# INLINE liftN #-}

-- | The family of /lifting/ functions. They trivially lift a computation over some effects into a larger effect
-- context. It can be also seen as masking a set of effects from the inner computation.
type LiftH eh' = ∀ eh ef a. HFunctors eh => Eff eh ef a -> Eff (eh' ++ eh) ef a

-- | Lift over some effects based on type inference. If this does not work consider using the more concrete functions
-- below.
liftH :: (Suffix eh eh', HFunctors eh) => Eff eh ef a -> Eff eh' ef a
liftH = alterRec Rec.suffix id
{-# INLINE liftH #-}

-- | Lift over 1 effect.
lift1H :: LiftH '[e']
lift1H = liftH
{-# INLINE lift1H #-}

-- | Lift over 2 effects.
lift2H :: Lift '[e', e'']
lift2H = lift
{-# INLINE lift2H #-}

-- | Lift over 3 effects.
lift3H :: Lift '[e', e'', e''']
lift3H = lift
{-# INLINE lift3H #-}

-- | Lift pver a list of effects supplied explicitly via @TypeApplications@.
liftNH :: ∀ ef'. KnownList ef' => Lift ef'
liftNH = alterRec id (Rec.drop @ef')
{-# INLINE liftNH #-}

--------------------------------------------------------------------------------
-- Lift Under ------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | The family of /lifting-under-1/ functions. They lift over several effects /under/ one effect.
type LiftNUnder ef' = ∀ e ef eh a. HFunctors eh => Eff eh (e ': ef) a -> Eff eh (e ': ef' ++ ef) a

-- | Lift over several effect under 1 effect, based on type inference. If this does not work consider using the more
-- concrete functions below.
liftUnder1 :: (Suffix ef ef', HFunctors eh) => Eff eh (e : ef) a -> Eff eh (e : ef') a
liftUnder1 = alterRec id (\es -> Rec.cons (Rec.head es) $ Rec.suffix es)
{-# INLINE liftUnder1 #-}

-- | Lift over 1 effect under 1 effect.
lift1Under1 :: LiftNUnder '[e']
lift1Under1 = liftUnder1
{-# INLINE lift1Under1 #-}

-- | The family of /lifting-under-1/ functions. They lift over several effects /under/ one effect.
type LiftNUnderH eh' = ∀ e eh ef a. (HFunctors eh, HFunctor e) => Eff (e : eh) ef a -> Eff (e : eh' ++ eh) ef a

-- | Lift over several effect under 1 effect, based on type inference. If this does not work consider using the more
-- concrete functions below.
liftUnder1H :: (Suffix eh eh', HFunctor e, HFunctors eh) => Eff (e : eh) ef a -> Eff (e : eh') ef a
liftUnder1H = alterRec (\es -> Rec.cons (Rec.head es) $ Rec.suffix es) id
{-# INLINE liftUnder1H #-}

-- | Lift over 1 effect under 1 effect.
lift1Under1H :: LiftNUnderH '[e']
lift1Under1H = liftUnder1H
{-# INLINE lift1Under1H #-}

{-
-- | Lift over 2 effects under 1 effect.
lift2Under1 :: LiftNUnder '[e', e'']
lift2Under1 = liftUnder1
{-# INLINE lift2Under1 #-}

-- | Lift over 3 effects under 1 effect.
lift3Under1 :: LiftNUnder '[e', e'', e''']
lift3Under1 = liftUnder1
{-# INLINE lift3Under1 #-}

-- | Lift over a list of effects under 1 effect. The list of effects is supplied explicitly via @TypeApplications@.
liftNUnder1 :: ∀ es'. KnownList es' => LiftNUnder es'
liftNUnder1 = alterRec (\es -> Rec.cons (Rec.head es) $ Rec.drop @(_ : es') es) id
{-# INLINE liftNUnder1 #-}

-- | The family of /lifting-1-under/ functions. They lift over an effect /under several effects/. This family of
-- functions don't have inferred variants because they're hard to formulate.
type LiftUnderN es' = ∀ e es ef a. HFunctors (es' ++ es) => Eff (es' ++ es) ef a -> Eff (es' ++ e : es) ef a

-- | Lift over 1 effect under 2 effects.
lift1Under2 :: ∀ e' e''. LiftUnderN '[e', e'']
lift1Under2 = lift1UnderN @'[e', e'']
{-# INLINE lift1Under2 #-}

-- | Lift over 1 effect under 3 effects.
lift1Under3 :: ∀ e' e'' e'''. LiftUnderN '[e', e'', e''']
lift1Under3 = lift1UnderN @'[e', e'', e''']
{-# INLINE lift1Under3 #-}

-- | Lift over 1 effect under a list effects explicitly supplied via @TypeApplications@.
lift1UnderN :: ∀ es' e es ef a. (KnownList es', HFunctors (es' ++ es)) => Eff (es' ++ es) ef a -> Eff (es' ++ e : es) ef a
lift1UnderN = alterRec (\es -> Rec.concat (Rec.take @es' @(e : es) es) $ Rec.tail $ Rec.drop @es' @(e : es) es) id
{-# INLINE lift1UnderN #-}
-}

-- | The most general /lifting-under/ function. Lifts over a list of effects under another list of effects. Both
-- lists are to supplied explicitly via @TypeApplications@.
liftNUnderN :: ∀ ef'' ef' ef eh a. (KnownList ef', KnownList ef'', HFunctors eh) => Eff eh (ef' ++ ef) a -> Eff eh (ef' ++ ef'' ++ ef) a
liftNUnderN = alterRec id (\ef -> Rec.concat (Rec.take @ef' @(ef'' ++ ef) ef) $ Rec.drop @ef'' @ef $ Rec.drop @ef' @(ef'' ++ ef) ef)
{-# INLINE liftNUnderN #-}

-- | The most general /lifting-under/ function. Lifts over a list of effects under another list of effects. Both
-- lists are to supplied explicitly via @TypeApplications@.
liftNUnderNH :: ∀ eh'' eh' eh ef a. (KnownList eh', KnownList eh'', HFunctors (eh' ++ eh)) => Eff (eh' ++ eh) ef a -> Eff (eh' ++ eh'' ++ eh) ef a
liftNUnderNH = alterRec (\eh -> Rec.concat (Rec.take @eh' @(eh'' ++ eh) eh) $ Rec.drop @eh'' @eh $ Rec.drop @eh' @(eh'' ++ eh) eh) id
{-# INLINE liftNUnderNH #-}

{-

--------------------------------------------------------------------------------
-- Subsume ---------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | The family of /subsumption/ functions. They trivially eliminate duplicate effects from the top of the stack. This
-- functions don't have inferred variants because they're hard to formulate.
type Subsume es' = ∀ es ef a. (KnownSubset es' es, HFunctors (es' ++ es)) => Eff (es' ++ es) ef a -> Eff es ef a

-- | Subsume 1 duplicate effect.
subsume1 :: ∀ e'. Subsume '[e']
subsume1 = subsumeN @'[e']
{-# INLINE subsume1 #-}

-- | Subsume 2 duplicate effects.
subsume2 :: ∀ e' e''. Subsume '[e', e'']
subsume2 = subsumeN @'[e', e'']
{-# INLINE subsume2 #-}

-- | Subsume 3 duplicate effects.
subsume3 :: ∀ e' e'' e'''. Subsume '[e', e'', e''']
subsume3 = subsumeN @'[e', e'', e''']
{-# INLINE subsume3 #-}

-- | Subsume a list duplicate effects explicitly supplied via @TypeApplications@.
subsumeN :: ∀ es'. KnownList es' => Subsume es'
subsumeN = alterRec (\es -> Rec.concat (Rec.pick @es' es) es) id
{-# INLINE subsumeN #-}

--------------------------------------------------------------------------------
-- Miscellaneous ---------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Inject a small effect context with all elements known into a larger effect context.
inject :: (KnownSubset es' es, HFunctors es') => Eff es' ef a -> Eff es ef a
inject = alterRec Rec.pick id
{-# INLINE inject #-}

-- | Arbitrarily rearrange the known prefix of the effect context of a computation, as long as the polymorphic tail
-- is unchanged. This function is based on type inference, use 'inject' or 'rearrangeN' when it doesn't work properly.
rearrange :: (Subset es es', HFunctors es) => Eff es ef a -> Eff es' ef a
rearrange = alterRec Rec.extract id
{-# INLINE rearrange #-}

-- | Like 'rearrange' but with the prefixes explicitly supplied via @TypeApplications@.
rearrangeN :: ∀ es' es'' es ef a. (KnownList es'', KnownSubset es' (es'' ++ es), HFunctors (es' ++ es)) => Eff (es' ++ es) ef a -> Eff (es'' ++ es) ef a
rearrangeN = alterRec (\es -> Rec.concat (Rec.pick @es' es) $ Rec.drop @es'' @es es) id
{-# INLINE rearrangeN #-}
-}
