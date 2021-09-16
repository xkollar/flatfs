{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main (main) where

import GHC.Num (fromInteger)
import GHC.Real (divMod)

import Control.Applicative (Applicative, pure)
import Control.Monad ((>>=), when)
import Control.Monad.Fail (fail)
import Data.Binary (encode)
import Data.Eq ((/=))
import Data.Function (($), flip)
import Data.Functor ((<$>))
import Data.Maybe (Maybe(Nothing), maybe)
import Data.Ord ((>))
import Data.Word (Word64)
import System.IO
    ( Handle, IO, IOMode(ReadWriteMode)
    , hFileSize, hTell, print, withBinaryFile
    )
import Text.Printf (printf)

import Options.Applicative (execParser)
import qualified Data.ByteString.Lazy as BSL (hPut)
import qualified Data.Set as Set (empty)
import Data.UUID.V4 (nextRandom)

import Lib
    ( BlockSize
    , Delimiters(Delimiters)
    , Header(Header, blockSize, blocks, version, label, uuid, superblockSize)
    , NextInodeBlock(NextInodeBlock)
    , Superblock(Superblock, header, delimiters, nextInodeBlock)
    , Version(Version)
    , fromBlockSize
    )

import Opts(Config(Config), blocks, blockSize, device, label, opts, superblockSize, uuid)


flatfs_version :: Version
flatfs_version = Version 0 1

fromMaybe :: Applicative f => f a -> Maybe a -> f a
fromMaybe = flip maybe pure

getBlocks :: Handle -> BlockSize -> IO Word64
getBlocks handle blockSize = do
    size <- fromInteger <$> hFileSize handle
    let bs = fromBlockSize blockSize
        (d, m) = size `divMod` bs
    when (m /= 0) $ printf "Block not aligned %d blocks of size %d and remainder %d\n" d bs m
    pure d
  where

makeFlat :: Config -> IO ()
makeFlat Config{..} = withBinaryFile device ReadWriteMode $ \ handle -> do
    givenUuid <- fromMaybe nextRandom uuid
    givenBlocks <- fromMaybe (getBlocks handle blockSize) blocks
    when (blockSize > superblockSize) $ fail "blockSize > superblockSize"
    let header = Header
            { version = flatfs_version
            , superblockSize
            , blockSize
            , blocks = givenBlocks
            , label
            , uuid = givenUuid
            }
        superblock = Superblock
            { header = header
            , delimiters = Delimiters Set.empty
            , nextInodeBlock = NextInodeBlock Nothing
            }
    print superblock
    BSL.hPut handle $ encode superblock
    hTell handle >>= print
    pure ()

main :: IO ()
main = execParser opts >>= makeFlat
