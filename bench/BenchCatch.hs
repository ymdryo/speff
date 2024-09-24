{-# LANGUAGE CPP #-}
-- Benchmarking scoped effects #1: Catching errors
module BenchCatch where

import qualified Control.Carrier.Error.Either as F
import qualified Control.Carrier.Reader       as F
#if SPEFF_BENCH_EFFECTFUL
import qualified Effectful                    as EL
import qualified Effectful.Error.Dynamic      as EL
import qualified Effectful.Reader.Dynamic     as EL
#endif
import qualified Polysemy                     as P
import qualified Polysemy.Error               as P
import qualified Polysemy.Reader              as P
import qualified Sp.Eff                       as S
import qualified Sp.Error                     as S
import qualified Sp.Reader                    as S

programSp :: (S.Throw () S.:> ef, S.Try () S.:> eh) => Int -> S.Eff eh ef a
programSp = \case
  0 -> S.throw ()
  n -> S.catch (programSp (n - 1)) \() -> S.throw ()
{-# NOINLINE programSp #-}

catchSp :: Int -> Either () ()
catchSp n = S.runEff $ S.runThrow $ S.runTry @() $ programSp n

catchSpDeep0 :: Int -> Either () ()
catchSpDeep0 n = S.runEff $ run $ run $ run $ run $ run $ S.runThrow $ run $ run $ run $ run $ run $ S.runTry @() $ programSp n
  where
    run :: S.Eff eh (S.Ask () ': ef) a -> S.Eff eh ef a
    run = S.runAsk ()

catchSpDeep1 :: Int -> Either () ()
catchSpDeep1 n = S.runEff $ run $ run $ run $ run $ run $ S.runThrow $ run $ run $ run $ run $ S.runTry @() $ run $ programSp n
  where
    run :: S.Eff eh (S.Ask () ': ef) a -> S.Eff eh ef a
    run = S.runAsk ()

catchSpDeep2 :: Int -> Either () ()
catchSpDeep2 n = S.runEff $ run $ run $ run $ run $ run $ S.runThrow $ run $ run $ run $ S.runTry @() $ run $ run $ programSp n
  where
    run :: S.Eff eh (S.Ask () ': ef) a -> S.Eff eh ef a
    run = S.runAsk ()

catchSpDeep3 :: Int -> Either () ()
catchSpDeep3 n = S.runEff $ run $ run $ run $ run $ run $ S.runThrow $ run $ run $ S.runTry @() $ run $ run $ run $ programSp n
  where
    run :: S.Eff eh (S.Ask () ': ef) a -> S.Eff eh ef a
    run = S.runAsk ()

catchSpDeep4 :: Int -> Either () ()
catchSpDeep4 n = S.runEff $ run $ run $ run $ run $ run $ S.runThrow $ run $ S.runTry @() $ run $ run $ run $ run $ programSp n
  where
    run :: S.Eff eh (S.Ask () ': ef) a -> S.Eff eh ef a
    run = S.runAsk ()

catchSpDeep5 :: Int -> Either () ()
catchSpDeep5 n = S.runEff $ run $ run $ run $ run $ run $ S.runThrow $ S.runTry @() $ run $ run $ run $ run $ run $ programSp n
  where
    run :: S.Eff eh (S.Ask () ': ef) a -> S.Eff eh ef a
    run = S.runAsk ()

#if SPEFF_BENCH_EFFECTFUL
programEffectful :: EL.Error () EL.:> es => Int -> EL.Eff es a
programEffectful = \case
  0 -> EL.throwError ()
  n -> EL.catchError (programEffectful (n - 1)) \_ () -> EL.throwError ()
{-# NOINLINE programEffectful #-}

catchEffectful :: Int -> Either (EL.CallStack, ()) ()
catchEffectful n = EL.runPureEff $ EL.runError $ programEffectful n

catchEffectfulDeep :: Int -> Either (EL.CallStack, ()) ()
catchEffectfulDeep n =
  EL.runPureEff $ run $ run $ run $ run $ run $ EL.runError $ run $ run $ run $ run $ run $ programEffectful n
  where run = EL.runReader ()
#endif

programFused :: F.Has (F.Error ()) sig m => Int -> m a
programFused = \case
  0 -> F.throwError ()
  n -> F.catchError (programFused (n - 1)) \() -> F.throwError ()
{-# NOINLINE programFused #-}

catchFused :: Int -> Either () ()
catchFused n = F.run $ F.runError $ programFused n

catchFusedDeep :: Int -> Either () ()
catchFusedDeep n = F.run $ run $ run $ run $ run $ run $ F.runError $ run $ run $ run $ run $ run $ programFused n
  where run = F.runReader ()

programSem :: P.Error () `P.Member` es => Int -> P.Sem es a
programSem = \case
  0 -> P.throw ()
  n -> P.catch (programSem (n - 1)) \() -> P.throw ()
{-# NOINLINE programSem #-}

catchSem :: Int -> Either () ()
catchSem n = P.run $ P.runError $ programSem n

catchSemDeep :: Int -> Either () ()
catchSemDeep n = P.run $ run $ run $ run $ run $ run $ P.runError $ run $ run $ run $ run $ run $ programSem n
  where run = P.runReader ()
