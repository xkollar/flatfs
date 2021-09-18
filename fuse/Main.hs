module Main where

import Control.Applicative (pure)
import Data.Function (($), (.))
import Data.Either (Either(Left, Right))
import Data.List (foldr1)
import System.IO (FilePath, IO, putStrLn)

import System.Fuse
    ( EntryType(Directory, RegularFile)
    , Errno
    , FileStat(FileStat)
    , FuseContext
    , FuseOperations
    , defaultExceptionHandler
    , defaultFuseOps
    , eNOENT
    , eOK
    , fuseCtxGroupID
    , fuseCtxUserID
    , fuseGetFileStat
    , fuseMain
    , fuseOpenDirectory
    , fuseReadDirectory
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
import System.Posix.Files (unionFileModes, ownerReadMode, ownerExecuteMode, groupReadMode, groupExecuteMode, otherReadMode, otherExecuteMode)


data State = State

data Handle = Handle

type FuseOut a = IO (Either Errno a)

fail :: Errno -> FuseOut a
fail = pure . Left

ok :: a -> FuseOut a
ok = pure . Right

mkState :: IO State
mkState = pure State

main :: IO ()
main = do
    putStrLn "Starting"
    st <- mkState
    fuseMain (flatFuseOps st) defaultExceptionHandler

flatFuseOps :: State -> FuseOperations Handle
flatFuseOps _ = defaultFuseOps
    { fuseGetFileStat = flatGetFileStat
    , fuseOpenDirectory = flatOpenDirectory
    , fuseReadDirectory = flatReadDirectory
    }

dirStat :: FuseContext -> FileStat
dirStat ctx = FileStat
    { statEntryType = Directory
    , statFileMode = foldr1 unionFileModes
        [ ownerReadMode
        , ownerExecuteMode
        , groupReadMode
        , groupExecuteMode
        , otherReadMode
        , otherExecuteMode
        ]
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

fileStat :: FuseContext -> FileStat
fileStat ctx = FileStat
    { statEntryType = RegularFile
    , statFileMode = foldr1 unionFileModes
        [ ownerReadMode
        , groupReadMode
        , otherReadMode
        ]
    , statLinkCount = 1
    , statFileOwner = fuseCtxUserID ctx
    , statFileGroup = fuseCtxGroupID ctx
    , statSpecialDeviceID = 0
    , statFileSize = 0
    , statBlocks = 1
    , statAccessTime = 0
    , statModificationTime = 0
    , statStatusChangeTime = 0
    }


flatGetFileStat :: FilePath -> FuseOut FileStat
flatGetFileStat "/" = do
    ctx <- getFuseContext
    ok $ dirStat ctx
flatGetFileStat _ =
    fail eNOENT

flatReadDirectory :: FilePath -> FuseOut [(FilePath, FileStat)]
flatReadDirectory "/" = do
    ctx <- getFuseContext
    ok
        [ (".", dirStat ctx)
        , ("..", dirStat ctx)
        ]
flatReadDirectory _ = fail eNOENT

flatOpenDirectory :: FilePath -> IO Errno
flatOpenDirectory "/" = pure eOK
flatOpenDirectory _   = pure eNOENT
