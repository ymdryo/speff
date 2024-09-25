{-# LANGUAGE PackageImports #-}

module BenchCoroutine where

import qualified Sp.Eff                        as S
import qualified Sp.Reader                     as S
import qualified Sp.Coroutine                  as S
import qualified "freer-simple" Control.Monad.Freer           as FS
import qualified "freer-simple"Control.Monad.Freer.Coroutine as FS
import qualified "freer-simple"Control.Monad.Freer.Reader as FS
import qualified "hefty-freer-simple" Control.Monad.Freer           as HF
import qualified "hefty-freer-simple"Control.Monad.Freer.Coroutine as HF
import qualified "hefty-freer-simple"Control.Monad.Freer.Reader as HF
import qualified Control.Mp.Eff as M
import qualified Control.Mp.Util as M
import Control.Monad (forM)
import qualified Control.Effect as E

programSp :: S.Yield Int Int S.:> ef => Int -> S.Eff eh ef [Int]
programSp upbound =
    forM [1..upbound] \i -> S.yield i
{-# NOINLINE programSp #-}

loopStatusSp :: S.Status (S.Eff eh ef) Int Int r -> S.Eff eh ef r
loopStatusSp = \case
    S.Done r -> pure r
    S.Continue i f -> loopStatusSp =<< f (i+100)
{-# NOINLINE loopStatusSp #-}

coroutineSp :: Int -> [Int]
coroutineSp n = S.runEff $ loopStatusSp =<< S.runCoroutine (programSp n)

coroutineSpDeep :: Int -> [Int]
coroutineSpDeep n = S.runEff $ run $ run $ run $ run $ run $ loopStatusSp =<< S.runCoroutine (run $ run $ run $ run $ run $ programSp n)
  where run = S.runAsk ()


programMp :: M.Yield Int Int M.:? e => Int -> M.Eff e [Int]
programMp n = forM [0..n] $ \i -> M.perform M.yield i
{-# NOINLINE programMp #-}

loopStatusMp :: M.Status e Int Int r -> M.Eff e r
loopStatusMp = \case
    M.Done r -> pure r
    M.Continue a k -> loopStatusMp =<< k (a+100)
{-# NOINLINE loopStatusMp #-}

coroutineMp :: Int -> [Int]
coroutineMp n = M.runEff $ loopStatusMp =<< M.coroutine @Int @Int (programMp n)

coroutineMpDeep :: Int -> [Int]
coroutineMpDeep n = M.runEff $ run $ run $ run $ run $ run $ loopStatusMp =<< M.coroutine @Int @Int (run $ run $ run $ run $ run $ programMp n)
  where run = M.reader ()


programFreer :: FS.Member (FS.Yield Int Int) es => Int -> FS.Eff es [Int]
programFreer upbound =
    forM [1..upbound] \i -> FS.yield i id
{-# NOINLINE programFreer #-}

loopStatusFreer :: FS.Status es Int Int r -> FS.Eff es r
loopStatusFreer = \case
    FS.Done r -> pure r
    FS.Continue i f -> loopStatusFreer =<< f (i+100)
{-# NOINLINE loopStatusFreer #-}

coroutineFreer :: Int -> [Int]
coroutineFreer n = FS.run $ loopStatusFreer =<< FS.runC (programFreer n)

coroutineFreerDeep :: Int -> [Int]
coroutineFreerDeep n = FS.run $ run $ run $ run $ run $ run $ loopStatusFreer =<< FS.runC (run $ run $ run $ run $ run $ programFreer n)
  where run = FS.runReader ()

programHeftyFreer :: HF.Member (HF.Yield Int Int) es => Int -> HF.Eff '[] es [Int]
programHeftyFreer upbound =
    forM [1..upbound] \i -> HF.yield i id
{-# NOINLINE programHeftyFreer #-}

loopStatusHeftyFreer :: HF.Status es Int Int r -> HF.Eff '[] es r
loopStatusHeftyFreer = \case
    HF.Done r -> pure r
    HF.Continue i f -> loopStatusHeftyFreer =<< f (i+100)
{-# NOINLINE loopStatusHeftyFreer #-}

coroutineHeftyFreer :: Int -> [Int]
coroutineHeftyFreer n = HF.run $ loopStatusHeftyFreer =<< HF.runC (programHeftyFreer n)

coroutineHeftyFreerDeep :: Int -> [Int]
coroutineHeftyFreerDeep n = HF.run $ run $ run $ run $ run $ run $ loopStatusHeftyFreer =<< HF.runC (run $ run $ run $ run $ run $ programHeftyFreer n)
  where run = HF.runReader ()

programEff :: E.Coroutine Int Int E.:< es => Int -> E.Eff es [Int]
programEff upbound =
    forM [1..upbound] \i -> E.yield @Int @Int i
{-# NOINLINE programEff #-}

loopStatusEff :: E.Status es Int Int r -> E.Eff es r
loopStatusEff = \case
    E.Done r -> pure r
    E.Yielded i f -> loopStatusEff =<< E.runCoroutine (f (i+100))
{-# NOINLINE loopStatusEff #-}

coroutineEff :: Int -> [Int]
coroutineEff n = E.run $ loopStatusEff =<< E.runCoroutine (programEff n)

coroutineEffDeep :: Int -> [Int]
coroutineEffDeep n = E.run $ run $ run $ run $ run $ run $ loopStatusEff =<< E.runCoroutine (run $ run $ run $ run $ run $ programEff n)
  where run = E.runReader ()
