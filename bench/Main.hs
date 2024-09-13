{-# LANGUAGE CPP #-}
module Main where

import           BenchCountdown
import           Data.Functor     ((<&>))
import           Test.Tasty.Bench

main :: IO ()
main = defaultMain
  [ bgroup "countdown" $ [10000] <&> \x -> bgroup (show x)
    [ bench "sp.shallow" $ nf countdownSp x
    , bench "sp.deep" $ nf countdownSpDeep x
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
  ]
