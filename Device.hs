module Device where

import Data.Array.IO
import Data.Array.Unboxed
import Data.Word

import Data.ByteString

type Size = Word64
data Device = Device
    { s :: Size
    , d :: IOUArray Size Word
    }

mkDevice :: Word64 -> IO Device
mkDevice size = Device size <$> newArray (0, size-1) 0

-- size :: Device -> IO Size
-- size = pure . uncurry (-) . bounds . un

main :: IO ()
main = do
    a <- mkDevice (100*10^2)
    print ()
