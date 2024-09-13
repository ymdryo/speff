{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- Benchmarking effect invocation and monadic bind
module BenchCountdown where

import Control.Carrier.Reader qualified as F
import Control.Carrier.State.Strict qualified as F
import Control.Ev.Eff qualified as E
import Control.Ev.Util qualified as E
#ifdef SPEFF_BENCH_FREER_SIMPLE
import qualified Control.Monad.Freer          as FS
import qualified Control.Monad.Freer.Reader   as FS
import qualified Control.Monad.Freer.State    as FS
#endif
import Control.Monad.Identity qualified as M
import Control.Monad.Reader qualified as M
import Control.Monad.State.Strict qualified as M
#if SPEFF_BENCH_EFFECTFUL
import qualified Effectful                    as EL
import qualified Effectful.Reader.Dynamic     as EL
import qualified Effectful.State.Dynamic      as EL
#endif
import Polysemy qualified as P
import Polysemy.Reader qualified as P
import Polysemy.State qualified as P
import Sp.Eff qualified as S
import Sp.Internal.Monad qualified as S
import Sp.Reader qualified as S
import Sp.State qualified as S

programSp :: S.State Int S.:> es => S.Eff IO es Int
programSp = do
    x <- S.get @Int
    if x == 0
        then pure x
        else do
            S.put (x - 1)
            programSp
{-# NOINLINE programSp #-}

countdownSp :: Int -> (Int, Int)
countdownSp n = S.runPure' $ S.runState n programSp

countdownSpDeep :: Int -> (Int, Int)
countdownSpDeep n = S.runPure' $ runR $ runR $ runR $ runR $ runR $ S.runState n $ runR $ runR $ runR $ runR $ runR $ programSp
  where
    runR = S.runReader ()

#if SPEFF_BENCH_EFFECTFUL
programEffectful :: EL.State Int EL.:> es => EL.Eff es Int
programEffectful = do
  x <- EL.get @Int
  if x == 0
    then pure x
    else do
      EL.put (x - 1)
      programEffectful
{-# NOINLINE programEffectful #-}

countdownEffectful :: Int -> (Int, Int)
countdownEffectful n = EL.runPureEff $ EL.runStateLocal n programEffectful

countdownEffectfulDeep :: Int -> (Int, Int)
countdownEffectfulDeep n =
  EL.runPureEff $ runR $ runR $ runR $ runR $ runR $ EL.runStateLocal n $ runR $ runR $ runR $ runR $ runR $ programEffectful
  where runR = EL.runReader ()
#endif

programEv :: E.State Int E.:? es => E.Eff es Int
programEv = do
    x <- E.perform (E.get @Int) ()
    if x == 0
        then pure x
        else do
            E.perform E.put (x - 1)
            programEv
{-# NOINLINE programEv #-}

countdownEv :: Int -> Int
countdownEv n = E.runEff $ E.state n programEv

countdownEvDeep :: Int -> Int
countdownEvDeep n = E.runEff $ runR $ runR $ runR $ runR $ runR $ E.state n $ runR $ runR $ runR $ runR $ runR $ programEv
  where
    runR = E.reader ()

#ifdef SPEFF_BENCH_FREER_SIMPLE
programFreer :: FS.Member (FS.State Int) es => FS.Eff es Int
programFreer = do
  x <- FS.get @Int
  if x == 0
    then pure x
    else do
      FS.put (x - 1)
      programFreer
{-# NOINLINE programFreer #-}

countdownFreer :: Int -> (Int, Int)
countdownFreer n = FS.run $ FS.runState n programFreer

countdownFreerDeep :: Int -> (Int, Int)
countdownFreerDeep n = FS.run $ runR $ runR $ runR $ runR $ runR $ FS.runState n $ runR $ runR $ runR $ runR $ runR $ programFreer
  where runR = FS.runReader ()
#endif

programMtl :: M.MonadState Int m => m Int
programMtl = do
    x <- M.get @Int
    if x == 0
        then pure x
        else do
            M.put (x - 1)
            programMtl
{-# NOINLINE programMtl #-}

countdownMtl :: Int -> (Int, Int)
countdownMtl n = M.runState programMtl n

countdownMtlDeep :: Int -> (Int, Int)
countdownMtlDeep n = M.runIdentity $ runR $ runR $ runR $ runR $ runR $ M.runStateT (runR $ runR $ runR $ runR $ runR $ programMtl) n
  where
    runR = (`M.runReaderT` ())

programFused :: F.Has (F.State Int) sig m => m Int
programFused = do
    x <- F.get @Int
    if x == 0
        then pure x
        else do
            F.put (x - 1)
            programFused
{-# NOINLINE programFused #-}

countdownFused :: Int -> (Int, Int)
countdownFused n = F.run $ F.runState n programFused

countdownFusedDeep :: Int -> (Int, Int)
countdownFusedDeep n = F.run $ runR $ runR $ runR $ runR $ runR $ F.runState n $ runR $ runR $ runR $ runR $ runR $ programFused
  where
    runR = F.runReader ()

programSem :: P.Member (P.State Int) es => P.Sem es Int
programSem = do
    x <- P.get @Int
    if x == 0
        then pure x
        else do
            P.put (x - 1)
            programSem
{-# NOINLINE programSem #-}

countdownSem :: Int -> (Int, Int)
countdownSem n = P.run $ P.runState n programSem

countdownSemDeep :: Int -> (Int, Int)
countdownSemDeep n = P.run $ runR $ runR $ runR $ runR $ runR $ P.runState n $ runR $ runR $ runR $ runR $ runR $ programSem
  where
    runR = P.runReader ()
