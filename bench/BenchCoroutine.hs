module BenchCoroutine where

import qualified Sp.Eff                        as S
import qualified Sp.Reader                     as S
import qualified Sp.Coroutine                  as S
import qualified Control.Monad.Freer           as FS
import qualified Control.Monad.Freer.Coroutine as FS
import Control.Monad (forM)
import qualified Control.Monad.Freer.Reader as FS

{-
programSp :: (S.Coroutine S.:> e) => Int -> S.Eff e (Int, Int, Int)
programSp upbound = do
  x <- S.choice [1..upbound]
  y <- S.choice [1..upbound]
  z <- S.choice [1..upbound]
  if x*x + y*y == z*z then return (x,y,z) else S.send S.Empty
{-# NOINLINE programSp #-}

pythSp :: Int -> [(Int, Int, Int)]
pythSp n = S.runEff $ S.runNonDet $ programSp n

pythSpDeep :: Int -> [(Int, Int, Int)]
pythSpDeep n = S.runEff $ run $ run $ run $ run $ run $ S.runNonDet $ run $ run $ run $ run $ run $ programSp n
  where run = S.runReader ()
-}

programSp :: S.Yield Int Int S.:> es => Int -> S.Eff es [Int]
programSp upbound =
    forM [1..upbound] \i -> S.yield i
{-# NOINLINE programSp #-}

loopStatusSp :: S.Status es Int Int r -> S.Eff es r
loopStatusSp = \case
    S.Done r -> pure r
    S.Continue i f -> loopStatusSp =<< f (i+100)

coroutineSp :: Int -> [Int]
coroutineSp n = S.runEff $ loopStatusSp =<< S.runCoroutine (programSp n)

coroutineSpDeep :: Int -> [Int]
coroutineSpDeep n = S.runEff $ run $ run $ run $ run $ run $ loopStatusSp =<< S.runCoroutine (run $ run $ run $ run $ run $ programSp n)
  where run = S.runReader ()


programFreer :: FS.Member (FS.Yield Int Int) es => Int -> FS.Eff es [Int]
programFreer upbound =
    forM [1..upbound] \i -> FS.yield i id
{-# NOINLINE programFreer #-}

loopStatusFreer :: FS.Status es Int Int r -> FS.Eff es r
loopStatusFreer = \case
    FS.Done r -> pure r
    FS.Continue i f -> loopStatusFreer =<< f (i+100)

coroutineFreer :: Int -> [Int]
coroutineFreer n = FS.run $ loopStatusFreer =<< FS.runC (programFreer n)

coroutineFreerDeep :: Int -> [Int]
coroutineFreerDeep n = FS.run $ run $ run $ run $ run $ run $ loopStatusFreer =<< FS.runC (run $ run $ run $ run $ run $ programFreer n)
  where run = FS.runReader ()
