{-# LANGUAGE CPP #-}
module Main where

import           BenchCatch
-- import           BenchCountdown
import           BenchLocal
import           BenchPyth
import           BenchCoroutine
import           Data.Functor     ((<&>))
import           Test.Tasty.Bench

main :: IO ()
main = defaultMain
  [ {-bgroup "countdown" $ [10000] <&> \x -> bgroup (show x)
    [ bench "sp_modified_for_non_scoped_resumption_support.shallow" $ nf countdownSp x
    , bench "sp_modified_for_non_scoped_resumption_support.deep" $ nf countdownSpDeep x
#if SPEFF_BENCH_EFFECTFUL
    , bench "effectful.shallow" $ nf countdownEffectful x
    , bench "effectful.deep" $ nf countdownEffectfulDeep x
#endif
    , bench "ev.shallow" $ nf countdownEv x
    , bench "ev.deep" $ nf countdownEvDeep x
#if SPEFF_BENCH_FREER_SIMPLE
    , bench "freer.shallow" $ nf countdownFreer x
    , bench "freer.deep" $ nf countdownFreerDeep x
#endif
    , bench "mtl.shallow" $ nf countdownMtl x
    , bench "mtl.deep" $ nf countdownMtlDeep x
    , bench "fused.shallow" $ nf countdownFused x
    , bench "fused.deep" $ nf countdownFusedDeep x
    , bench "sem.shallow" $ nf countdownSem x
    , bench "sem.deep" $ nf countdownSemDeep x
    ]
  , -} bgroup "pyth" $ [32] <&> \x -> bgroup (show x)
    [ bench "sp_modified_for_non_scoped_resumption_support.shallow" $ nf pythSp x
    , bench "sp_modified_for_non_scoped_resumption_support.deep" $ nf pythSpDeep x
    , bench "ev.shallow" $ nf pythEv x
    , bench "ev.deep" $ nf pythEvDeep x
#ifdef SPEFF_BENCH_FREER_SIMPLE
    , bench "freer.shallow" $ nf pythFreer x
    , bench "freer.deep" $ nf pythFreerDeep x
#endif
    , bench "fused.shallow" $ nf pythFused x
    , bench "fused.deep" $ nf pythFusedDeep x
    , bench "sem.shallow" $ nf pythSem x
    , bench "sem.deep" $ nf pythSemDeep x
    ]
  , bgroup "catch" $ [10000] <&> \x -> bgroup (show x)
    [ bench "sp_modified_for_non_scoped_resumption_support.shallow" $ nf catchSp x
    , bench "sp_modified_for_non_scoped_resumption_support.deep" $ nf catchSpDeep x
#if SPEFF_BENCH_EFFECTFUL
    , bench "effectful.shallow" $ nf catchEffectful x
    , bench "effectful.deep" $ nf catchEffectfulDeep x
#endif
    , bench "fused.shallow" $ nf catchFused x
    , bench "fused.deep" $ nf catchFusedDeep x
    , bench "sem.shallow" $ nf catchSem x
    , bench "sem.deep" $ nf catchSemDeep x
    ]
  , bgroup "local" $ [10000] <&> \x -> bgroup (show x)
    [ bench "sp_modified_for_non_scoped_resumption_support.shallow" $ nf localSp x
    , bench "sp_modified_for_non_scoped_resumption_support.deep" $ nf localSpDeep x
#if SPEFF_BENCH_EFFECTFUL
    , bench "effectful.shallow" $ nf localEffectful x
    , bench "effectful.deep" $ nf localEffectfulDeep x
#endif
    , bench "fused.shallow" $ nf localFused x
    , bench "fused.deep" $ nf localFusedDeep x
    , bench "sem.shallow" $ nf localSem x
    , bench "sem.deep" $ nf localSemDeep x
    ]
  , bgroup "coroutine" $ [10000] <&> \x -> bgroup (show x)
    [ bench "sp_modified_for_non_scoped_resumption_support.shallow" $ nf coroutineSp x
    , bench "sp_modified_for_non_scoped_resumption_support.deep" $ nf coroutineSpDeep x
    , bench "mp.shallow" $ nf coroutineMp x
    , bench "mp.deep" $ nf coroutineMpDeep x
    , bench "freer.shallow" $ nf coroutineFreer x
    , bench "freer.deep" $ nf coroutineFreerDeep x
    , bench "eff.shallow" $ nf coroutineEff x
    , bench "eff.deep" $ nf coroutineEffDeep x
    ]
  ]
