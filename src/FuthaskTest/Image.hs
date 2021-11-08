{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}

module FuthaskTest.Image
  ( ImageFloat(..)
  , ImageWord8(..)
  , imageSize
  , readImageWord8Float
  , writeImageWord8Float
  , readImageWord16Float
  , writeImageWord16Float
  , readImage8Arr3d
  , writeImage8Arr3d
  , readImage16Arr3d
  , writeImage16Arr3d
  , imgRgbaToArr1d
  , arr1dToImgRgba
  , imgRgbaToArr3d
  , arr3dToImgRgba
  , fromMonoImg
  )
where

import qualified Data.Massiv.Array as A
import Data.Massiv.Array ( S(..)
                         , Comp(..)
                         , Sz1(..)
                         , Sz2(..)
                         , Sz3(..)
                         , pattern Sz1
                         , pattern Sz2
                         , pattern Sz3
                         , Ix1(..)
                         , Ix2(..)
                         , Ix3(..)
                         , pattern Ix1
                         , pattern Ix2
                         , pattern Ix3
                         , Load(..)
                         , Construct(..)
                         , Manifest(..)
                         , Array(..)
                         , (!)
                         )
import Data.Massiv.Array.IO
import qualified Graphics.ColorModel as CM
import Data.Word
import Data.Int

import FuthaskTest.Type

wORD8mAX  = 255
wORD16mAX = 65535

type ImageFloat = Image S (CM.Alpha CM.RGB) Float
type ImageWord8 = Image S (CM.Alpha CM.RGB) Word8

word8ToFloatPixel :: Pixel (CM.Alpha CM.RGB) Word8
                  -> Pixel (CM.Alpha CM.RGB) Float
word8ToFloatPixel = fmap (\ i -> fromIntegral i / wORD8mAX)

floatToWord8Pixel :: Pixel (CM.Alpha CM.RGB) Float
                  -> Pixel (CM.Alpha CM.RGB) Word8
floatToWord8Pixel = fmap (\ i -> round $ i * wORD8mAX)

word16ToFloatPixel :: Pixel (CM.Alpha CM.RGB) Word16
                   -> Pixel (CM.Alpha CM.RGB) Float
word16ToFloatPixel = fmap (\ i -> fromIntegral i / 65535)

floatToWord16Pixel :: Pixel (CM.Alpha CM.RGB) Float
                  -> Pixel (CM.Alpha CM.RGB) Word16
floatToWord16Pixel = fmap (\ i -> round $ i * 65535)

imageSize :: ImageFloat -> (Int64, Int64)
imageSize img =
  let (Sz2 h w) = size img
  in  (fromIntegral h, fromIntegral w)

readImageWord8Float :: String -> IO ImageFloat
readImageWord8Float fileName =
  do imgWord8 <- readImage fileName :: IO (Image S (CM.Alpha CM.RGB) Word8)
     return $ A.computeS $ A.map word8ToFloatPixel imgWord8

writeImageWord8Float :: String -> ImageFloat -> IO ()
writeImageWord8Float fileName img =
  let imgWord8 = A.computeS $ A.map floatToWord8Pixel img
  in  writeImage fileName (imgWord8 :: Image S (CM.Alpha CM.RGB) Word8)

readImageWord16Float :: String -> IO ImageFloat
readImageWord16Float fileName =
  do imgWord16 <- readImage fileName :: IO (Image S (CM.Alpha CM.RGB) Word16)
     return $ A.computeS $ A.map word16ToFloatPixel imgWord16

writeImageWord16Float :: String -> ImageFloat -> IO ()
writeImageWord16Float fileName img =
  let imgWord16 = A.computeS $ A.map floatToWord16Pixel img
  in  writeImage fileName (imgWord16 :: Image S (CM.Alpha CM.RGB) Word16)

readImage8Arr3d :: String -> IO Futhark3dFloat
readImage8Arr3d fileName =
  do img <- readImageWord8Float fileName
     return $ imgRgbaToArr3d img

writeImage8Arr3d :: String -> Futhark3dFloat -> IO ()
writeImage8Arr3d fileName arr =
  do let img = arr3dToImgRgba arr
     writeImageWord8Float fileName img

readImage16Arr3d :: String -> IO Futhark3dFloat
readImage16Arr3d fileName =
  do img <- readImageWord16Float fileName
     return $ imgRgbaToArr3d img

writeImage16Arr3d :: String -> Futhark3dFloat -> IO ()
writeImage16Arr3d fileName arr =
  do let img = arr3dToImgRgba arr
     writeImageWord16Float fileName img

imgRgbaToArr3d :: ( Load r' Ix3 e
                  , Load r Ix2 (Pixel (Alpha CM.RGB) e)
                  , Construct r' Ix3 e
                  , Manifest r Ix2 (Pixel (Alpha CM.RGB) e)
                  )
               => Array r  Ix2 (Pixel (Alpha CM.RGB) e)
               -> Array r' Ix3 e
imgRgbaToArr3d img =
  let (Sz2 imgHeight imgWidth) = size img
  in  makeArray Seq (Sz3 imgHeight imgWidth 4) $
      \ (Ix3 y x channel) ->
        let (CM.PixelRGBA r g b a) = img ! (Ix2 y x)
        in
        case channel of
          0 -> r
          1 -> g
          2 -> b
          3 -> a

imgRgbaToArr1d :: ( Load r' Ix3 e
                  , Load r Ix2 (Pixel (Alpha CM.RGB) e)
                  , Construct r' Ix3 e
                  , A.Resize r' Ix3
                  , Manifest r Ix2 (Pixel (Alpha CM.RGB) e)
                  )
               => Array r  Ix2 (Pixel (Alpha CM.RGB) e)
               -> Array r' Ix1 e
imgRgbaToArr1d =  A.flatten . imgRgbaToArr3d

arr3dToImgRgba :: ( Load r Ix2 (Pixel (Alpha CM.RGB) e)
                  , Load r' Ix3 e
                  , Construct r Ix2 (Pixel (Alpha CM.RGB) e)
                  , Manifest r' Ix3 e
                  )
               => Array r' Ix3 e
               -> Array r  Ix2 (Pixel (Alpha CM.RGB) e)
arr3dToImgRgba array =
  let (Sz3 arrayHeight arrayWidth _) = size array
  in makeArray Seq (Sz2 arrayHeight arrayWidth) $
       \ (Ix2 y x) ->
       let r = array ! (Ix3 y x 0)
           g = array ! (Ix3 y x 1)
           b = array ! (Ix3 y x 2)
           a = array ! (Ix3 y x 3)
       in  CM.PixelRGBA r g b a

arr1dToImgRgba :: ( Load r Ix2 (Pixel (Alpha CM.RGB) e)
                  , Load r' Ix1 e
                  , Construct r Ix2 (Pixel (Alpha CM.RGB) e)
                  , Manifest r' Ix3 e
                  , A.Resize r' Ix1
                  )
               => Int
               -> Int
               -> Array r' Ix1 e
               -> Array r  Ix2 (Pixel (Alpha CM.RGB) e)
arr1dToImgRgba width height = arr3dToImgRgba . A.resize' (Sz3 height width 4)

fromMonoImg ::( Load r' Ix2 e
              , Manifest r' Ix2 e
              , Integral e
              )
              => Array r' Ix2 e
              -> ImageFloat
fromMonoImg array =
  let (Sz2 arrayHeight arrayWidth) = size array
  in makeArray Seq (Sz2 arrayHeight arrayWidth) $
       \ (Ix2 y x) ->
       let gray = fromIntegral $ array ! (Ix2 y x)
       in  CM.PixelRGBA gray gray gray 1
