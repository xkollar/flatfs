{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module Lib
    ( Delimiters(..)
    , Header(..)
    , Label()
    , NextInodeBlock(..)
    , Superblock(..)
    , SuperblockSize(..)
    , Version(..)
    )
  where

import Prelude (Integral, (+), (-), (^), fromIntegral)

import Control.Applicative ((<*>), (*>), pure)
import Control.Monad ((>>), (>>=), when)
import Control.Monad.Fail (MonadFail, fail)
import Data.Binary (Binary(put, get), Get, getWord8, putWord8)
import Data.Binary.Get (getLazyByteString, bytesRead, isEmpty, isolate, skip)
import Data.Binary.Put (Put, putLazyByteString, runPut)
import Data.Bool (Bool(False, True))
import Data.Eq ((/=))
import Data.Foldable (for_)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Int (Int64)
import Data.List (filter)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid ((<>))
import Data.String (IsString(fromString))
import Data.Word (Word8, Word64)
import Text.Show (Show, show)
import Text.Printf (printf)

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL (length, repeat, replicate, take, takeWhile)
import Data.Set (Set)
import qualified Data.Set as Set (fromList, toAscList)
import Data.UUID (UUID)


data Version = Version
    { major :: Word8
    , minor :: Word8
    }
  deriving Show

instance Binary Version where
    put Version{..} = putWord8 major >> putWord8 minor
    get = Version <$> getWord8 <*> getWord8

newtype Label = Label ByteString
  deriving Show

instance IsString Label where
    fromString = Label . BSL.take 16 . fromString

instance Binary Label where
    put (Label l) = putLazyByteString (BSL.take 16 $ l <> BSL.repeat 0)
    get = Label . BSL.takeWhile (0 /=) <$> getLazyByteString 16

data SuperblockSize
    = Size128
    | Size256
    | Size512
    | Size1024
    | Size2048
  deriving Show

superblockSizeEncode :: SuperblockSize -> Word8
superblockSizeEncode = \case
    Size128 -> 0
    Size256  -> 1
    Size512  -> 2
    Size1024 -> 3
    Size2048 -> 4

superblockSizeDecode :: MonadFail m => Word8 -> m SuperblockSize
superblockSizeDecode = \case
    0 -> pure Size128
    1 -> pure Size256
    2 -> pure Size512
    3 -> pure Size1024
    4 -> pure Size2048
    x -> fail $ printf "Invalid superblockSize %d" x

fromSuperblockSize :: Integral a => SuperblockSize -> a
fromSuperblockSize size = 2^(7 + superblockSizeEncode size)

getSuperblockSize :: Integral a => Header -> a
getSuperblockSize = fromSuperblockSize . superblockSize

instance Binary SuperblockSize where
    put = put . superblockSizeEncode
    get = get >>= superblockSizeDecode

data Header = Header
    { version :: Version
    , size :: Word64
    , superblockSize :: SuperblockSize
    -- ^ (2 ^ (7 + min 4 Word8) - (58 + 6))
    --                             |    '- Padding
    --                             '- Size of header
    , label :: Label
    , uuid :: UUID
    }
  deriving Show

str :: ByteString -> Get ()
str s = do
    x <- getLazyByteString (BSL.length s)
    when (x /= s) . fail $ printf "Expected %s found %s" (show s) (show x)

padZeros :: Int64 -> Put -> Put
padZeros n p = do
    let payload = runPut p
        rest = n - BSL.length payload
    putLazyByteString payload
    putLazyByteString $ BSL.replicate rest 0

getAll :: Get a -> Get [a]
getAll g = f
  where
    f = isEmpty >>= \case
        True -> pure []
        False -> do
            x <- g
            s <- f
            pure (x:s)

instance Binary Header where
    put Header{..} = do
        putLazyByteString "FLAT"
        put version
        put size
        put superblockSize
        putLazyByteString "LABEL="
        put label
        putLazyByteString "UUID="
        put uuid
        putLazyByteString $ BSL.replicate 6 0
    get = do
        str "FLAT"
        ret <- Header <$> get <*> get <*> get <*> (str "LABEL=" *> get) <*> (str "UUID=" *> get)
        skip 6
        pure ret

newtype Delimiters = Delimiters (Set Word64)
  deriving Show

-- XXX lol needs plenty of work
instance Binary Delimiters where
    put (Delimiters s) = for_ (Set.toAscList s) put
    get = Delimiters . Set.fromList . filter (0 /=) <$> getAll get

newtype NextInodeBlock = NextInodeBlock (Maybe Word64)
  deriving Show

instance Binary NextInodeBlock where
    put (NextInodeBlock x) = case x of
        Nothing -> put (0 :: Word64)
        Just offset -> put offset
    get = get >>= pure . NextInodeBlock . \case
        0 -> Nothing
        x -> Just x

data Superblock = Superblock
    { header :: Header
    , delimiters :: Delimiters
    , nextInodeBlock :: NextInodeBlock
    }
  deriving Show

instance Binary Superblock where
    put Superblock{..} = do
      let sbs = getSuperblockSize header
      padZeros (sbs - 8) $ do
        put header
        put delimiters
      put nextInodeBlock
    get = do
      h <- get
      pos <- fromIntegral <$> bytesRead
      let sbs = getSuperblockSize h
      delims <- isolate (sbs - pos - 8) get
      Superblock h delims <$> get
