
module Futhark.Entries where
import qualified Futhark.Raw as Raw
import qualified Futhark.Context as C
import Futhark.Fut (FutT)
import qualified Futhark.Fut as Fut
import qualified Futhark.Wrap as U
import Futhark.Types
import qualified Futhark.TypeClasses as T
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import qualified Foreign as F
import Foreign.C.Types

addNoise
  :: Monad m 
  => Float
  -> Int32
  -> F32_3d c
  -> FutT c m (F32_3d c)
addNoise in0 in1 in2
  =  Fut.unsafeLiftFromIO $ \context
  -> T.withFO in2 $ \in2'
  -> F.malloc >>= \out0
  -> C.inContextWithError context (\context'
  -> Raw.entry_addNoise context' out0 in0 in1 in2')
  >> U.peekFreeWrapIn context out0
