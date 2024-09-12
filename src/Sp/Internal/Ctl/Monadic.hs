{- |
 Copyright: (c) 2024, Yamada Ryo. 2022, Xy Ren. 2021, Ningning Xie, Daan Leijen.
 License: MPL-2.0
 Maintainer: ymdfield@outlook.jp
 Stability: experimental
 Portability: non-portable (GHC only)
-}
module Sp.Internal.Ctl.Monadic where

import Control.Monad (ap, liftM, (<=<))
import Data.Functor.Identity (Identity)
import Data.Kind (Type)
import Data.Primitive.PrimVar (PrimVar, fetchAddInt, newPrimVar)
import Data.Type.Equality (type (:~:) (Refl))
import GHC.Exts (RealWorld)
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

type EffectH = (Type -> Type) -> Type -> Type
type Carrier = Type -> Type
