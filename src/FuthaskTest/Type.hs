{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE PatternSynonyms     #-}

module FuthaskTest.Type
    ( Futhark1dFloat(..)
    , Futhark2dFloat(..)
    , Futhark3dFloat(..)
    , Futhark1dInt64(..)
    , Futhark2dInt64(..)
    , Futhark3dInt64(..)
    , Futhark1dBool(..)
    , Futhark2dBool(..)
    , Futhark3dBool(..)
    , Array1d(..)
    , Array2d(..)
    , Array3d(..)
    )
where

import Data.Massiv.Array ( Array
                         , S
                         , B
                         , Ix1(..)
                         , Ix2(..)
                         , Ix3(..)
                         , Sz1(..)
                         , Sz2(..)
                         , Sz3(..)
                         , pattern Sz1
                         , pattern Sz2
                         , pattern Sz3
                         )
import Data.Int

-- Futhark return types

type Futhark1dFloat = Array S Ix1 Float
type Futhark2dFloat = Array S Ix2 Float
type Futhark3dFloat = Array S Ix3 Float
type Futhark1dInt64 = Array S Ix1 Int64
type Futhark2dInt64 = Array S Ix2 Int64
type Futhark3dInt64 = Array S Ix3 Int64
type Futhark1dBool  = Array S Ix1 Bool
type Futhark2dBool  = Array S Ix2 Bool
type Futhark3dBool  = Array S Ix3 Bool

type Array1d t = Array B Ix1 t
type Array2d t = Array B Ix2 t
type Array3d t = Array B Ix3 t
