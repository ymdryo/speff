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
import           Sp.Internal.Env   (KnownList, KnownSubset, Subset, Suffix, type (++), (:>))
import           Sp.Internal.Monad

--------------------------------------------------------------------------------
-- Interpret -------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | The family of /interpreting/ functions. Eliminate an effect from the top of the stack via a handler, and
-- optionally add some other effects (@es'@) that could be used in the handler. Adding these effects instead of
-- requiring them on the client side achieves effect encapsulation.
type Interpret ef' = ∀ e ef eh a. HFunctors eh => Handler e eh (ef' ++ ef) a -> Eff eh (e : ef) a -> Eff eh (ef' ++ ef) a

-- | The family of /interpreting/ functions. Eliminate an effect from the top of the stack via a handler, and
-- optionally add some other effects (@es'@) that could be used in the handler. Adding these effects instead of
-- requiring them on the client side achieves effect encapsulation.
type InterpretH eh' = ∀ e eh ef a. (HFunctors eh, HFunctor e) => Elaborator e (eh' ++ eh) ef a -> Eff (e ': eh) ef a -> Eff (eh' ++ eh) ef a

-- | Interpret and add extra effects based on type inference. If this does not work consider using the more concrete
-- functions below.
interpret :: (Suffix ef ef', HFunctors eh) => Handler e eh ef' a -> Eff eh (e : ef) a -> Eff eh ef' a
interpret = handle \ih -> alterEnvRec id (Rec.cons ih . Rec.suffix)
{-# INLINE interpret #-}

-- | Interpret and don't add extra effects.
interpret0 :: Interpret '[]
interpret0 = interpret
{-# INLINE interpret0 #-}

-- | Interpret and add 1 extra effect.
interpret1 :: Interpret '[e']
interpret1 = interpret
{-# INLINE interpret1 #-}

-- | Interpret and add 2 extra effects.
interpret2 :: Interpret '[e', e'']
interpret2 = interpret
{-# INLINE interpret2 #-}

-- | Interpret and add 3 extra effects.
interpret3 :: Interpret '[e', e'', e''']
interpret3 = interpret
{-# INLINE interpret3 #-}

-- | Interpret and add a list of extra effects specified explicitly via @TypeApplications@.
interpretN :: ∀ es'. KnownList es' => Interpret es'
interpretN = handle \ih -> alterEnvRec id (Rec.cons ih . Rec.drop @es')
{-# INLINE interpretN #-}

-- | Interpret and add extra effects based on type inference. If this does not work consider using the more concrete
-- functions below.
interpretH :: (Suffix eh eh', HFunctors eh, HFunctor e) => Elaborator e eh' ef a -> Eff (e ': eh) ef a -> Eff eh' ef a
interpretH = handleH \ie -> alterEnvRec (Rec.cons ie . Rec.suffix) id
{-# INLINE interpretH #-}

-- | Interpret and don't add extra effects.
interpret0H :: InterpretH '[]
interpret0H = interpretH
{-# INLINE interpret0H #-}

-- | Interpret and add 1 extra effect.
interpret1H :: InterpretH '[e']
interpret1H = interpretH
{-# INLINE interpret1H #-}

-- | Interpret and add 2 extra effects.
interpret2H :: InterpretH '[e', e'']
interpret2H = interpretH
{-# INLINE interpret2H #-}

-- | Interpret and add 3 extra effects.
interpret3H :: InterpretH '[e', e'', e''']
interpret3H = interpretH
{-# INLINE interpret3H #-}

-- | Interpret and add a list of extra effects specified explicitly via @TypeApplications@.
interpretNH :: ∀ es'. KnownList es' => InterpretH es'
interpretNH = handleH \ie -> alterEnvRec (Rec.cons ie . Rec.drop @es') id
{-# INLINE interpretNH #-}

--------------------------------------------------------------------------------
-- Interpose -------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | The family of /interposing/ functions. Modify the implementation of an effect in the stack via a new handler, and
-- optionally add some other effects (@es'@) that could be used in said handler.
type Interpose ef' = ∀ e ef eh a. (e :> ef, HFunctors eh) => Handler e eh (ef' ++ ef) a -> Eff eh ef a -> Eff eh (ef' ++ ef) a

-- | The family of /interposing/ functions. Modify the implementation of an effect in the stack via a new handler, and
-- optionally add some other effects (@es'@) that could be used in said handler.
type InterposeH eh' = ∀ e eh ef a. (e :> eh, HFunctors eh) => Elaborator e (eh' ++ eh) ef a -> Eff eh ef a -> Eff (eh' ++ eh) ef a

-- | Interpose and add extra effects based on type inference. If this does not work consider using the more concrete
-- functions below.
interpose :: (e :> ef, Suffix ef ef', HFunctors eh) => Handler e eh ef' a -> Eff eh ef a -> Eff eh ef' a
interpose = handle \ih -> alterEnvRec id (Rec.update ih . Rec.suffix)
{-# INLINE interpose #-}

-- | Interpose and don't add extra effects.
interpose0 :: Interpose '[]
interpose0 = interpose
{-# INLINE interpose0 #-}

-- | Interpose and add extra effects based on type inference. If this does not work consider using the more concrete
-- functions below.
interposeH :: (e :> eh, Suffix eh eh', HFunctors eh) => Elaborator e eh' ef a -> Eff eh ef a -> Eff eh' ef a
interposeH = handleH \ie -> alterEnvRec (Rec.update ie . Rec.suffix) id
{-# INLINE interposeH #-}

-- | Interpose and don't add extra effects.
interpose0H :: InterposeH '[]
interpose0H = interposeH
{-# INLINE interpose0H #-}

-- | Interpose and add 1 extra effect.
interpose1H :: InterposeH '[e']
interpose1H = interposeH
{-# INLINE interpose1H #-}

-- | Interpose and add 2 extra effects.
interpose2H :: InterposeH '[e', e'']
interpose2H = interposeH
{-# INLINE interpose2H #-}

-- | Interpose and add 3 extra effects.
interpose3H :: InterposeH '[e', e'', e''']
interpose3H = interposeH
{-# INLINE interpose3H #-}

-- | Interpose and add a list of extra effects specified explicitly via @TypeApplications@.
interposeNH :: ∀ es'. KnownList es' => InterposeH es'
interposeNH = handleH \ie -> alterEnvRec (Rec.update ie . Rec.drop @es') id
{-# INLINE interposeNH #-}

--------------------------------------------------------------------------------
-- Replace ---------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | The family of /interposing/ functions. Modify the implementation of an effect in the stack via a new handler, and
-- optionally add some other effects (@es'@) that could be used in said handler.
type ReplaceH eh' = ∀ e eh ef a. (e :> eh, HFunctors eh) => Elaborator e (eh' ++ eh) ef a -> Eff eh ef a -> Eff (eh' ++ eh) ef a

-- | Replace and add extra effects based on type inference. If this does not work consider using the more concrete
-- functions below.
replaceH :: (e :> es, Suffix es es', HFunctors es) => Elaborator e es' ef a -> Eff es ef a -> Eff es' ef a
replaceH = handleH \_ -> alterEnvRec Rec.suffix id
{-# INLINE replaceH #-}

-- | Replace and don't add extra effects.
replace0H :: ReplaceH '[]
replace0H = replaceH
{-# INLINE replace0H #-}

-- | Replace and add 1 extra effect.
replace1H :: ReplaceH '[e']
replace1H = replaceH
{-# INLINE replace1H #-}

-- | Replace and add 2 extra effects.
replace2H :: ReplaceH '[e', e'']
replace2H = replaceH
{-# INLINE replace2H #-}

-- | Replace and add 3 extra effects.
replace3H :: ReplaceH '[e', e'', e''']
replace3H = replaceH
{-# INLINE replace3H #-}

-- | Replace and add a list of extra effects specified explicitly via @TypeApplications@.
replaceNH :: ∀ es'. KnownList es' => ReplaceH es'
replaceNH = handleH \_ -> alterEnvRec (Rec.drop @es') id
{-# INLINE replaceNH #-}

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

{-
-- | The family of /lifting/ functions. They trivially lift a computation over some effects into a larger effect
-- context. It can be also seen as masking a set of effects from the inner computation.
type Lift es' = ∀ es ef a. HFunctors es => Eff es ef a -> Eff (es' ++ es) ef a

-- | Lift over some effects based on type inference. If this does not work consider using the more concrete functions
-- below.
lift :: (Suffix es es', HFunctors es) => Eff es ef a -> Eff es' ef a
lift = alterRec Rec.suffix id
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
liftN :: ∀ es'. KnownList es' => Lift es'
liftN = alterRec (Rec.drop @es') id
{-# INLINE liftN #-}
-}

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

{-
-- | The family of /lifting-under-1/ functions. They lift over several effects /under/ one effect.
type LiftNUnder es' = ∀ e es ef a. (HFunctors es, HFunctor e) => Eff (e : es) ef a -> Eff (e : es' ++ es) ef a

-- | Lift over several effect under 1 effect, based on type inference. If this does not work consider using the more
-- concrete functions below.
liftUnder1 :: (Suffix es es', HFunctor e, HFunctors es) => Eff (e : es) ef a -> Eff (e : es') ef a
liftUnder1 = alterRec (\es -> Rec.cons (Rec.head es) $ Rec.suffix es) id
{-# INLINE liftUnder1 #-}

-- | Lift over 1 effect under 1 effect.
lift1Under1 :: LiftNUnder '[e']
lift1Under1 = liftUnder1
{-# INLINE lift1Under1 #-}

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

-- | The most general /lifting-under/ function. Lifts over a list of effects under another list of effects. Both
-- lists are to supplied explicitly via @TypeApplications@.
liftNUnderN :: ∀ es'' es' es ef a. (KnownList es', KnownList es'', HFunctors (es' ++ es)) => Eff (es' ++ es) ef a -> Eff (es' ++ es'' ++ es) ef a
liftNUnderN = alterRec (\es ->
  Rec.concat (Rec.take @es' @(es'' ++ es) es) $ Rec.drop @es'' @es $ Rec.drop @es' @(es'' ++ es) es
    ) id
{-# INLINE liftNUnderN #-}

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
