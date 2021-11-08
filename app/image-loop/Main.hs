{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeApplications           #-}

module Main where

import System.Environment
import System.Directory
import Data.List
import Data.Int
import Control.Monad.Trans
import Control.Monad.Random.Class
import Control.Monad.Random

import qualified Data.Massiv.Array    as A

import Futhark
import Futhark.Types
import qualified Futhark.Entries as E
import FuthaskTest.Image
import FuthaskTest.Type


addNoiseToImage :: (MonadIO m)
                => FilePath
                -> FilePath
                -> FilePath
                -> FutT c (RandT StdGen m) ()
addNoiseToImage sourceFolder destinationFolder file =
  do  lift $ liftIO $ putStrLn file
      seed :: Int32 <- lift $ getRandom
      input  <- lift $ liftIO $ readImage8Arr3d (sourceFolder ++ file)
      futInput  <- toFuthark input
      futOutput <- E.addNoise 0.3 seed futInput
      (output :: Futhark3dFloat) <- fromFuthark futOutput
      lift $ liftIO $ writeImage8Arr3d (destinationFolder ++ file) output
      return ()

main :: IO ()
main = do args <- getArgs
          let (imageFolder, outputFolder) =
               case args of
                  i:o:_ -> (i,o)
                  [] -> error "Usage: image-loop [imageFolder] [outputPath]"
          files <- sort <$> listDirectory imageFolder
          gen <- getStdGen
          evalRandT (runFutT $ mapM_ (addNoiseToImage imageFolder outputFolder) files) gen
