{-# LANGUAGE RankNTypes, ExistentialQuantification, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module Futhark.Types where
import qualified Futhark.Raw as Raw
import Futhark.Wrap
import Futhark.TypeClasses
import qualified Foreign as F
import qualified Data.Massiv.Array as M
import qualified Control.Concurrent.MVar as MV
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.C.Types (CBool(..), CSize(..), CChar(..), CFile(..))
import Foreign.Ptr (Ptr)
import Control.DeepSeq (rwhnf)

data F32_3d c = F32_3d (MV.MVar Int) (F.ForeignPtr Raw.Futhark_f32_3d)
instance FutharkObject F32_3d Raw.Futhark_f32_3d where
  wrapFO = F32_3d
  freeFO = Raw.free_f32_3d
  fromFO (F32_3d rc fp) = (rc, fp)
instance FutharkArray F32_3d Raw.Futhark_f32_3d M.Ix3 Float where
  shapeFA  = to3d Raw.shape_f32_3d
  newFA    = from3d Raw.new_f32_3d
  valuesFA = Raw.values_f32_3d
