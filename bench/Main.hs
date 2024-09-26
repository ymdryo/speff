{-# LANGUAGE CPP #-}
module Main where

import           BenchCatch
import           BenchCountdown
import           BenchLocal
import           BenchPyth
import           BenchCoroutine
import           Data.Functor     ((<&>))
import           Test.Tasty.Bench

main :: IO ()
main = defaultMain
  [ bgroup "countdown" $ [10000] <&> \x -> bgroup (show x)
    [ bench "sp_modified_for_non_scoped_resumption_support.shallow" $ nf countdownSp x
    , bench "sp_modified_for_non_scoped_resumption_support.deep" $ nf countdownSpDeep x
    , bench "sp_modified_for_non_scoped_resumption_support.IO.shallow" $ nf countdownSpIO x
    , bench "sp_modified_for_non_scoped_resumption_support.IO.deep" $ nf countdownSpDeepIO x
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
    , bench "hefty-freer.shallow" $ nf countdownHeftyFreer x
    , bench "hefty-freer.deep" $ nf countdownHeftyFreerDeep x
    , bench "hefty-freer.IO.shallow" $ nf countdownHeftyFreerIO x
    , bench "hefty-freer.IO.deep" $ nf countdownHeftyFreerDeepIO x
    , bench "mtl.shallow" $ nf countdownMtl x
    , bench "mtl.deep" $ nf countdownMtlDeep x
    , bench "fused.shallow" $ nf countdownFused x
    , bench "fused.deep" $ nf countdownFusedDeep x
    , bench "sem.shallow" $ nf countdownSem x
    , bench "sem.deep" $ nf countdownSemDeep x
    ]
  , bgroup "pyth" $ [32] <&> \x -> bgroup (show x)
    [ bench "sp_modified_for_non_scoped_resumption_support.shallow" $ nf pythSp x
    , bench "sp_modified_for_non_scoped_resumption_support.deep" $ nf pythSpDeep x
    , bench "ev.shallow" $ nf pythEv x
    , bench "ev.deep" $ nf pythEvDeep x
#ifdef SPEFF_BENCH_FREER_SIMPLE
    , bench "freer.shallow" $ nf pythFreer x
    , bench "freer.deep" $ nf pythFreerDeep x
#endif
    , bench "hefty-freer.shallow" $ nf pythHeftyFreer x
    , bench "hefty-freer.deep" $ nf pythHeftyFreerDeep x
    , bench "fused.shallow" $ nf pythFused x
    , bench "fused.deep" $ nf pythFusedDeep x
    , bench "sem.shallow" $ nf pythSem x
    , bench "sem.deep" $ nf pythSemDeep x
    ]
  , bgroup "catch" $ [10000] <&> \x -> bgroup (show x)
    [ bench "sp_modified_for_non_scoped_resumption_support.shallow" $ nf catchSp x
    , bench "sp_modified_for_non_scoped_resumption_support.deep0" $ nf catchSpDeep0 x
    , bench "sp_modified_for_non_scoped_resumption_support.deep1" $ nf catchSpDeep1 x
    , bench "sp_modified_for_non_scoped_resumption_support.deep2" $ nf catchSpDeep2 x
    , bench "sp_modified_for_non_scoped_resumption_support.deep3" $ nf catchSpDeep3 x
    , bench "sp_modified_for_non_scoped_resumption_support.deep4" $ nf catchSpDeep4 x
    , bench "sp_modified_for_non_scoped_resumption_support.deep5" $ nf catchSpDeep5 x
    , bench "hefty-freer.shallow" $ nf catchHeftyFreer x
    , bench "hefty-freer.deep0" $ nf catchHeftyFreerDeep0 x
    , bench "hefty-freer.deep1" $ nf catchHeftyFreerDeep1 x
    , bench "hefty-freer.deep2" $ nf catchHeftyFreerDeep2 x
    , bench "hefty-freer.deep3" $ nf catchHeftyFreerDeep3 x
    , bench "hefty-freer.deep4" $ nf catchHeftyFreerDeep4 x
    , bench "hefty-freer.deep5" $ nf catchHeftyFreerDeep5 x
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
    , bench "sp_modified_for_non_scoped_resumption_support.deep0" $ nf localSpDeep0 x
    , bench "sp_modified_for_non_scoped_resumption_support.deep1" $ nf localSpDeep1 x
    , bench "sp_modified_for_non_scoped_resumption_support.deep2" $ nf localSpDeep2 x
    , bench "sp_modified_for_non_scoped_resumption_support.deep3" $ nf localSpDeep3 x
    , bench "sp_modified_for_non_scoped_resumption_support.deep4" $ nf localSpDeep4 x
    , bench "sp_modified_for_non_scoped_resumption_support.deep5" $ nf localSpDeep5 x
    , bench "hefty-freer.shallow" $ nf localHeftyFreer x
    , bench "hefty-freer.deep0" $ nf localHeftyFreerDeep0 x
    , bench "hefty-freer.deep1" $ nf localHeftyFreerDeep1 x
    , bench "hefty-freer.deep2" $ nf localHeftyFreerDeep2 x
    , bench "hefty-freer.deep3" $ nf localHeftyFreerDeep3 x
    , bench "hefty-freer.deep4" $ nf localHeftyFreerDeep4 x
    , bench "hefty-freer.deep5" $ nf localHeftyFreerDeep5 x
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
    , bench "hefty-freer.shallow" $ nf coroutineHeftyFreer x
    , bench "hefty-freer.deep" $ nf coroutineHeftyFreerDeep x
    , bench "eff.shallow" $ nf coroutineEff x
    , bench "eff.deep" $ nf coroutineEffDeep x
    ]
  ]
