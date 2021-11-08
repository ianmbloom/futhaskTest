
module Futhark.Context where
import qualified Futhark.Raw as Raw
import Futhark.Config
import Foreign as F
import qualified Foreign.Concurrent as FC
import Foreign.C
import Control.Concurrent
import System.Mem (performGC)

data Context = Context (MVar Int) (ForeignPtr Raw.Futhark_context)

getContext :: [ContextOption] -> IO Context
getContext options = do
    config <- Raw.context_config_new
    mapM_ (setOption config) options
    context <- Raw.context_new config
    Raw.context_config_free config
    childCount <- newMVar 0
    fmap (Context childCount)
        $ FC.newForeignPtr context 
        $ (forkIO $ freeContext childCount context)
        >> return ()

freeContext childCount pointer 
    = readMVar childCount >>= \n 
    -> if n == 0 
        then Raw.context_free pointer 
        else yield >> freeContext childCount pointer

inContext (Context _ fp) = withForeignPtr fp

getError context = do
    cs <- inContext context Raw.context_get_error
    s <- peekCString cs
    F.free cs
    error s

clearError context = inContext context Raw.context_get_error >>= F.free

clearCache context
    = inContext context Raw.context_clear_caches >>= \code 
    -> if code == 0 
        then return ()
        else getError context

syncContext context 
    = inContext context Raw.context_sync >>= \code 
    -> if code == 0 
        then return ()
        else getError context

inContextWithError :: Context -> (Ptr Raw.Futhark_context -> IO Int) -> IO ()
inContextWithError context f = do
    code <- attempt
    if code == 0 
        then success
        else do
            clearError context
            performGC
            code' <- attempt
            if code' == 0
                then success
                else failure
    where
        attempt = inContext context f
        success = return ()
        failure = getError context

