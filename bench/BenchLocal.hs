{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
-- Benchmarking scoped effects #2: Local environments
module BenchLocal where

import qualified Control.Carrier.Error.Either as F
import qualified Control.Carrier.Reader       as F
#if SPEFF_BENCH_EFFECTFUL
import qualified Effectful                    as EL
import qualified Effectful.Reader.Dynamic     as EL
#endif
import qualified Polysemy                     as P
import qualified Polysemy.Reader              as P
import qualified Sp.Eff                       as S
import qualified Sp.Reader                    as S
import qualified "hefty-freer-simple" Control.Monad.Freer    as HF
import qualified "hefty-freer-simple" Control.Monad.Freer.Reader    as HF

programSp :: (S.Ask Int S.:> ef, S.Local Int S.:> eh) => Int -> S.Eff eh ef Int
programSp = \case
  0 -> S.ask
  n -> S.local @Int (+1) (programSp (n - 1))
{-# NOINLINE programSp #-}

localSp :: Int -> Int
localSp n = S.runEff $ S.runAsk @Int 0 $ S.runLocal @Int $ programSp n

localSpDeep0 :: Int -> Int
localSpDeep0 n = S.runEff $ run $ run $ run $ run $ run $ S.runAsk @Int 0 $ run $ run $ run $ run $ run $ S.runLocal @Int $ programSp n
  where
    run :: S.Eff eh (S.Ask () ': ef) a -> S.Eff eh ef a
    run = S.runAsk ()

localSpDeep1 :: Int -> Int
localSpDeep1 n = S.runEff $ run $ run $ run $ run $ run $ S.runAsk @Int 0 $ run $ run $ run $ run $ S.runLocal @Int $ run $ programSp n
  where
    run :: S.Eff eh (S.Ask () ': ef) a -> S.Eff eh ef a
    run = S.runAsk ()

localSpDeep2 :: Int -> Int
localSpDeep2 n = S.runEff $ run $ run $ run $ run $ run $ S.runAsk @Int 0 $ run $ run $ run $ S.runLocal @Int $ run $ run $ programSp n
  where
    run :: S.Eff eh (S.Ask () ': ef) a -> S.Eff eh ef a
    run = S.runAsk ()

localSpDeep3 :: Int -> Int
localSpDeep3 n = S.runEff $ run $ run $ run $ run $ run $ S.runAsk @Int 0 $ run $ run $ S.runLocal @Int $ run $ run $ run $ programSp n
  where
    run :: S.Eff eh (S.Ask () ': ef) a -> S.Eff eh ef a
    run = S.runAsk ()

localSpDeep4 :: Int -> Int
localSpDeep4 n = S.runEff $ run $ run $ run $ run $ run $ S.runAsk @Int 0 $ run $ S.runLocal @Int $ run $ run $ run $ run $ programSp n
  where
    run :: S.Eff eh (S.Ask () ': ef) a -> S.Eff eh ef a
    run = S.runAsk ()

localSpDeep5 :: Int -> Int
localSpDeep5 n = S.runEff $ run $ run $ run $ run $ run $ S.runAsk @Int 0 $ S.runLocal @Int $ run $ run $ run $ run $ run $ programSp n
  where
    run :: S.Eff eh (S.Ask () ': ef) a -> S.Eff eh ef a
    run = S.runAsk ()

programHeftyFreer :: (HF.Member (HF.Reader Int) ef, HF.MemberH (HF.Local Int) eh) => Int -> HF.Eff eh ef Int
programHeftyFreer = \case
  0 -> HF.ask
  n -> HF.sendH $ HF.Local @Int (+1) (programHeftyFreer (n - 1))
{-# NOINLINE programHeftyFreer #-}

localHeftyFreer :: Int -> Int
localHeftyFreer n = HF.run $ HF.runReader @Int 0 $ HF.runLocal @Int $ programHeftyFreer n

localHeftyFreerDeep0 :: Int -> Int
localHeftyFreerDeep0 n = HF.run $ run $ run $ run $ run $ run $ HF.runReader @Int 0 $ run $ run $ run $ run $ run $ HF.runLocal @Int $ programHeftyFreer n
  where
    run :: HF.Eff eh (HF.Reader () ': ef) a -> HF.Eff eh ef a
    run = HF.runReader ()

localHeftyFreerDeep1 :: Int -> Int
localHeftyFreerDeep1 n = HF.run $ run $ run $ run $ run $ run $ HF.runReader @Int 0 $ run $ run $ run $ run $ HF.runLocal @Int $ run $ programHeftyFreer n
  where
    run :: HF.Eff eh (HF.Reader () ': ef) a -> HF.Eff eh ef a
    run = HF.runReader ()

localHeftyFreerDeep2 :: Int -> Int
localHeftyFreerDeep2 n = HF.run $ run $ run $ run $ run $ run $ HF.runReader @Int 0 $ run $ run $ run $ HF.runLocal @Int $ run $ run $ programHeftyFreer n
  where
    run :: HF.Eff eh (HF.Reader () ': ef) a -> HF.Eff eh ef a
    run = HF.runReader ()

localHeftyFreerDeep3 :: Int -> Int
localHeftyFreerDeep3 n = HF.run $ run $ run $ run $ run $ run $ HF.runReader @Int 0 $ run $ run $ HF.runLocal @Int $ run $ run $ run $ programHeftyFreer n
  where
    run :: HF.Eff eh (HF.Reader () ': ef) a -> HF.Eff eh ef a
    run = HF.runReader ()

localHeftyFreerDeep4 :: Int -> Int
localHeftyFreerDeep4 n = HF.run $ run $ run $ run $ run $ run $ HF.runReader @Int 0 $ run $ HF.runLocal @Int $ run $ run $ run $ run $ programHeftyFreer n
  where
    run :: HF.Eff eh (HF.Reader () ': ef) a -> HF.Eff eh ef a
    run = HF.runReader ()

localHeftyFreerDeep5 :: Int -> Int
localHeftyFreerDeep5 n = HF.run $ run $ run $ run $ run $ run $ HF.runReader @Int 0 $ HF.runLocal @Int $ run $ run $ run $ run $ run $ programHeftyFreer n
  where
    run :: HF.Eff eh (HF.Reader () ': ef) a -> HF.Eff eh ef a
    run = HF.runReader ()

#if SPEFF_BENCH_EFFECTFUL
programEffectful :: EL.Reader Int EL.:> es => Int -> EL.Eff es Int
programEffectful = \case
  0 -> EL.ask
  n -> EL.local @Int (+1) (programEffectful (n - 1))
{-# NOINLINE programEffectful #-}

localEffectful :: Int -> Int
localEffectful n = EL.runPureEff $ EL.runReader @Int 0 $ programEffectful n

localEffectfulDeep :: Int -> Int
localEffectfulDeep n =
  EL.runPureEff $ run $ run $ run $ run $ run $ EL.runReader @Int 0 $ run $ run $ run $ run $ run $ programEffectful n
  where run = EL.runReader ()
#endif

programFused :: F.Has (F.Reader Int) sig m => Int -> m Int
programFused = \case
  0 -> F.ask
  n -> F.local @Int (+1) (programFused (n - 1))
{-# NOINLINE programFused #-}

localFused :: Int -> Int
localFused n = F.run $ F.runReader @Int 0 $ programFused n

localFusedDeep :: Int -> Int
localFusedDeep n = F.run $ run $ run $ run $ run $ run $ F.runReader @Int 0 $ run $ run $ run $ run $ run $ programFused n
  where run = F.runReader ()

programSem :: P.Reader Int `P.Member` es => Int -> P.Sem es Int
programSem = \case
  0 -> P.ask
  n -> P.local @Int (+1) (programSem (n - 1))
{-# NOINLINE programSem #-}

localSem :: Int -> Int
localSem n = P.run $ P.runReader @Int 0 $ programSem n

localSemDeep :: Int -> Int
localSemDeep n = P.run $ run $ run $ run $ run $ run $ P.runReader @Int 0 $ run $ run $ run $ run $ run $ programSem n
  where run = P.runReader ()
