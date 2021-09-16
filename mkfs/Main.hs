{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Control.Applicative (pure)
import Control.Monad ((>>=))
import Data.Binary (encode)
import Data.Function (($))
import Data.Maybe (Maybe(Nothing))
import System.IO (FilePath, IO, IOMode(ReadWriteMode), hTell, print, putStrLn, withBinaryFile)
import System.Environment (getArgs)
import Text.Show (Show)

import qualified Data.ByteString.Lazy as BSL (hPut)
import qualified Data.Set as Set (empty)
import Data.UUID.V4 (nextRandom)

import Lib
    ( Delimiters(Delimiters)
    , Header(Header, blockSize, blocks, version, label, uuid, superblockSize)
    , NextInodeBlock(NextInodeBlock)
    , BlockSize(Size512, Size1024)
    , Superblock(Superblock, header, delimiters, nextInodeBlock)
    , Version(Version)
    )


data Config = Config
    { device :: FilePath
    }
  deriving Show

flatfs_version :: Version
flatfs_version = Version 0 1

makeFlat :: Config -> IO ()
makeFlat config = withBinaryFile (device config) ReadWriteMode $ \ handle -> do
    freshUuid <- nextRandom
    let label = "plots"
        header = Header
            { version = flatfs_version
            , superblockSize = Size512
            , blockSize = Size1024
            , blocks = 1048576
            , label = label
            , uuid = freshUuid
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

main' :: [FilePath] -> IO ()
main' [filepath] =  makeFlat $ Config {device = filepath}
main' _ = putStrLn "BarArg"

main :: IO ()
main = getArgs >>= main'
