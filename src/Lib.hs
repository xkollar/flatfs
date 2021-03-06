{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module Lib
    ( BlockSize(..)
    , Delimiters(..)
    , Header(..)
    , Label(..)
    , NextExtentMapBlock(..)
    , Superblock(..)
    , Version(..)
    , fromBlockSize
    , toNextExtentMapBlock
    )
  where


import Prelude (Integral, (+), (-), (^), fromIntegral)
import GHC.Generics (Generic)
import GHC.Enum (Bounded, Enum)

import Control.Applicative ((<*>), (*>), pure)
import Control.Monad ((>>), (>>=), when)
import Control.Monad.Fail (MonadFail, fail)
import Data.Binary (Binary(put, get), Get, getWord8, putWord8)
import Data.Binary.Get (getLazyByteString, bytesRead, isEmpty, isolate, skip)
import Data.Binary.Put (Put, putLazyByteString, runPut)
import Data.Bool (Bool(False, True))
import Data.Eq (Eq, (/=))
import Data.Foldable (for_)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Int (Int64)
import Data.List (filter)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid ((<>))
import Data.Ord (Ord)
import Data.String (IsString(fromString))
import Data.Word (Word8, Word64)
import Text.Read (Read)
import Text.Show (Show, show)
import Text.Printf (printf)

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL (filter, length, repeat, replicate, take, takeWhile)
import Data.Set (Set)
import qualified Data.Set as Set (fromList, toAscList)
import Data.UUID (UUID)


data Version = Version
    { major :: Word8
    , minor :: Word8
    }
  deriving (Eq, Generic, Show)

instance Binary Version where
    put Version{..} = putWord8 major >> putWord8 minor
    get = Version <$> getWord8 <*> getWord8

newtype Label = Label ByteString
  deriving stock Generic
  deriving newtype (Eq, Show)

instance IsString Label where
    fromString = Label . BSL.take 16 . BSL.filter (0 /=) . fromString

instance Binary Label where
    put (Label l) = putLazyByteString (BSL.take 16 $ l <> BSL.repeat 0)
    get = Label . BSL.takeWhile (0 /=) <$> getLazyByteString 16

data BlockSize
    = Size128
    | Size256
    | Size512
    | Size1024
    | Size2048
    | Size4096
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

blockSizeEncode :: BlockSize -> Word8
blockSizeEncode = \case
    Size128  -> 0
    Size256  -> 1
    Size512  -> 2
    Size1024 -> 3
    Size2048 -> 4
    Size4096 -> 5

blockSizeDecode :: MonadFail m => Word8 -> m BlockSize
blockSizeDecode = \case
    0 -> pure Size128
    1 -> pure Size256
    2 -> pure Size512
    3 -> pure Size1024
    4 -> pure Size2048
    5 -> pure Size4096
    x -> fail $ printf "Invalid BlockSize %d" x

fromBlockSize' :: (Integral a, Integral b) => a -> b
fromBlockSize' x = 2^(7 + x)

fromBlockSize :: Integral a => BlockSize -> a
fromBlockSize = fromBlockSize' . blockSizeEncode

instance Binary BlockSize where
    put = put . blockSizeEncode
    get = get >>= blockSizeDecode

getSuperblockSize :: Integral a => Header -> a
getSuperblockSize = fromBlockSize . superblockSize

data Header = Header
    { version :: Version
    , superblockSize :: BlockSize
    , blockSize :: BlockSize
    , blocks :: Word64
    , label :: Label
    , uuid :: UUID
    }
  deriving (Eq, Generic, Show)

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
        False -> (:) <$> g <*> f

superblockMagic :: ByteString
superblockMagic = "FLAT"

labelMagic :: ByteString
labelMagic = "LABEL="

uuidMagic :: ByteString
uuidMagic = "UUID="

instance Binary Header where
    put Header{..} = do
        putLazyByteString superblockMagic
        put version
        put superblockSize
        put blockSize
        put blocks
        putLazyByteString labelMagic
        put label
        putLazyByteString uuidMagic
        put uuid
        putLazyByteString $ BSL.replicate 5 0
    get = do
        str superblockMagic
        ret <- Header
                <$> get
                <*> get
                <*> get
                <*> get
                <*> (str labelMagic *> get)
                <*> (str uuidMagic *> get)
        skip 5
        pure ret

newtype Delimiters = Delimiters (Set Word64)
  deriving stock Generic
  deriving newtype (Eq, Show)

instance Binary Delimiters where
    put (Delimiters s) = for_ (Set.toAscList s) put
    get = Delimiters . Set.fromList . filter (0 /=) <$> getAll get

newtype NextExtentMapBlock = NextExtentMapBlock (Maybe Word64)
  deriving stock Generic
  deriving newtype (Eq, Show)

toNextExtentMapBlock :: Word64 -> NextExtentMapBlock
toNextExtentMapBlock = NextExtentMapBlock . \case
    0 -> Nothing
    x -> Just x

instance Binary NextExtentMapBlock where
    put (NextExtentMapBlock x) = case x of
        Nothing -> put (0 :: Word64)
        Just offset -> put offset
    get = toNextExtentMapBlock <$> get

data Superblock = Superblock
    { header :: Header
    , delimiters :: Delimiters
    , nextInodeBlock :: NextExtentMapBlock
    }
  deriving stock Generic
  deriving (Eq, Show)

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
