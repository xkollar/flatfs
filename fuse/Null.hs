{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import GHC.Real (fromIntegral)

import Control.Applicative (pure)
import Control.Monad ((>>=))
import Data.Either (Either(Left, Right))
import Data.Function (($), (.))
import Data.List (foldr1)
import System.IO (FilePath, IO, putStrLn)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import System.Fuse
    ( EntryType(Directory)
    , Errno
    , FileStat(FileStat)
    , FuseContext
    , FuseOperations
    , OpenFileFlags
    , OpenMode(WriteOnly)
    , defaultExceptionHandler
    , defaultFuseOps
    , eNOENT
    , eOK
    , fuseCreateDevice
    , fuseCtxGroupID
    , fuseCtxUserID
    , fuseGetFileStat
    , fuseMain
    , fuseOpen
    , fuseOpenDirectory
    , fuseRead
    , fuseReadDirectory
    , fuseRelease
    , fuseWrite
    , getFuseContext
    , statAccessTime
    , statBlocks
    , statEntryType
    , statFileGroup
    , statFileMode
    , statFileOwner
    , statFileSize
    , statLinkCount
    , statModificationTime
    , statSpecialDeviceID
    , statStatusChangeTime
    )
import System.Posix.Files (unionFileModes, ownerReadMode, ownerWriteMode, ownerExecuteMode, groupReadMode, groupExecuteMode, otherReadMode, otherExecuteMode)
import System.Posix.Types (ByteCount, DeviceID, FileMode, FileOffset)

type Handle = ()

type FuseOut a = IO (Either Errno a)
type FuseRet = IO Errno

fail :: Errno -> FuseOut a
fail = pure . Left

ok :: a -> FuseOut a
ok = pure . Right

mode :: FileMode
mode = foldr1 unionFileModes
        [ ownerReadMode
        , ownerWriteMode
        , ownerExecuteMode
        , groupReadMode
        , groupExecuteMode
        , otherReadMode
        , otherExecuteMode
        ]

dirStat :: FuseContext -> FileStat
dirStat ctx = FileStat
    { statEntryType = Directory
    , statFileMode = mode
    , statLinkCount = 2
    , statFileOwner = fuseCtxUserID ctx
    , statFileGroup = fuseCtxGroupID ctx
    , statSpecialDeviceID = 0
    , statFileSize = 4096
    , statBlocks = 1
    , statAccessTime = 0
    , statModificationTime = 0
    , statStatusChangeTime = 0
    }

flatGetFileStat :: FilePath -> FuseOut FileStat
flatGetFileStat _ = getFuseContext >>= ok . dirStat

flatOpenDirectory :: FilePath -> FuseRet
flatOpenDirectory _ = pure eOK

flatReadDirectory :: FilePath -> FuseOut [(FilePath, FileStat)]
flatReadDirectory _ = do
    ctx <- getFuseContext
    ok [(".", dirStat ctx) , ("..", dirStat ctx)]

flatOpen :: FilePath -> OpenMode -> OpenFileFlags -> FuseOut Handle
flatOpen _ WriteOnly _ = ok ()
flatOpen _ _ _ = fail eNOENT

-- This should never happen...
flatRead :: FilePath -> Handle -> ByteCount -> FileOffset -> FuseOut ByteString
flatRead _ _ _ _ = fail eNOENT

flatWrite :: FilePath -> Handle -> ByteString -> FileOffset -> FuseOut ByteCount
flatWrite _ () bs _ = ok . fromIntegral $ BS.length bs

flatClose :: FilePath -> Handle -> IO ()
flatClose _ () = pure ()

flatCreateDevice :: FilePath -> EntryType -> FileMode -> DeviceID -> FuseRet
flatCreateDevice _ _ _ _ = pure eOK

flatFuseOps :: FuseOperations Handle
flatFuseOps = defaultFuseOps
    {
    -- fuseGetFileStat :: FilePath -> IO (Either Errno FileStat)
      fuseGetFileStat = flatGetFileStat
    -- fuseReadSymbolicLink :: FilePath -> IO (Either Errno FilePath)
    -- fuseCreateDevice :: FilePath -> EntryType -> FileMode -> DeviceID -> IO Errno
    , fuseCreateDevice = flatCreateDevice
    -- fuseCreateDirectory :: FilePath -> FileMode -> IO Errno
    -- fuseRemoveLink :: FilePath -> IO Errno
    -- fuseRemoveDirectory :: FilePath -> IO Errno
    -- fuseCreateSymbolicLink :: FilePath -> FilePath -> IO Errno
    -- fuseRename :: FilePath -> FilePath -> IO Errno
    -- fuseCreateLink :: FilePath -> FilePath -> IO Errno
    -- fuseSetFileMode :: FilePath -> FileMode -> IO Errno
    -- fuseSetOwnerAndGroup :: FilePath -> UserID -> GroupID -> IO Errno
    -- fuseSetFileSize :: FilePath -> FileOffset -> IO Errno
    -- fuseSetFileTimes :: FilePath -> EpochTime -> EpochTime -> IO Errno
    -- fuseOpen :: FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno fh)
    , fuseOpen = flatOpen
    -- fuseRead :: FilePath -> fh -> ByteCount -> FileOffset -> IO (Either Errno ByteString)
    , fuseRead = flatRead
    -- fuseWrite :: FilePath -> fh -> ByteString -> FileOffset -> IO (Either Errno ByteCount)
    , fuseWrite = flatWrite
    -- fuseGetFileSystemStats :: String -> IO (Either Errno FileSystemStats)
    -- fuseFlush :: FilePath -> fh -> IO Errno
    -- fuseRelease :: FilePath -> fh -> IO ()
    , fuseRelease = flatClose
    -- fuseSynchronizeFile :: FilePath -> SyncType -> IO Errno
    -- fuseOpenDirectory :: FilePath -> IO Errno
    , fuseOpenDirectory = flatOpenDirectory
    -- fuseReadDirectory :: FilePath -> IO (Either Errno [(FilePath, FileStat)])
    , fuseReadDirectory = flatReadDirectory
    -- fuseReleaseDirectory :: FilePath -> IO Errno
    -- fuseSynchronizeDirectory :: FilePath -> SyncType -> IO Errno
    -- fuseAccess :: FilePath -> Int -> IO Errno
    -- fuseInit :: IO ()
    -- fuseDestroy :: IO ()
    }

main :: IO ()
main = do
    putStrLn "Starting"
    fuseMain flatFuseOps defaultExceptionHandler
