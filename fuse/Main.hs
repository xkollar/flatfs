{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import GHC.Real (fromIntegral)

import Control.Applicative (pure)
import Control.Monad ((>>=))
import Data.Bool (Bool(True), otherwise)
import Data.Either (Either(Left, Right))
import Data.Eq (Eq, (==))
import Data.Function (($), (.))
import Data.Functor ((<$>), fmap)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (foldr1)
import qualified Data.List as List
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid ((<>))
import System.IO (FilePath, IO, putStrLn)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import System.Fuse
    ( EntryType(Directory, RegularFile)
    , Errno
    , FileStat(FileStat)
    , FuseContext
    , FuseOperations
    , OpenFileFlags(OpenFileFlags)
    , OpenMode(ReadOnly, ReadWrite, WriteOnly)
    , append
    , defaultExceptionHandler
    , defaultFuseOps
    , eACCES
    , eEXIST
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


type File = ByteString
type FS = [(FilePath, File)]
newtype State = State (IORef FS)

data Handle = Handle OpenMode

type FuseOut a = IO (Either Errno a)
type FuseRet = IO Errno

fail :: Errno -> FuseOut a
fail = pure . Left

ok :: a -> FuseOut a
ok = pure . Right

mkState :: IO State
mkState = State <$> newIORef
    [ ("a", "xa\n")
    , ("b", "xb\n")
    , ("wow", "omg\n")
    ]

dirStat :: FuseContext -> FileStat
dirStat ctx = FileStat
    { statEntryType = Directory
    , statFileMode = foldr1 unionFileModes
        [ ownerReadMode
        , ownerWriteMode
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

fileStat :: File -> FuseContext -> FileStat
fileStat f ctx = FileStat
    { statEntryType = RegularFile
    , statFileMode = foldr1 unionFileModes
        [ ownerReadMode
        ]
    , statLinkCount = 1
    , statFileOwner = fuseCtxUserID ctx
    , statFileGroup = fuseCtxGroupID ctx
    , statSpecialDeviceID = 0
    , statFileSize = fromIntegral (BS.length f)
    , statBlocks = 1
    , statAccessTime = 0
    , statModificationTime = 0
    , statStatusChangeTime = 0
    }

flatGetFileStat :: State -> FilePath -> FuseOut FileStat
flatGetFileStat _ "/" = getFuseContext >>= ok . dirStat
flatGetFileStat (State st) ('/':fileName) = do
    fs <- readIORef st
    putStrLn fileName
    case List.lookup fileName fs of
        Just f -> getFuseContext >>= ok . fileStat f
        Nothing -> fail eNOENT
flatGetFileStat _ _ = fail eNOENT

flatOpenDirectory :: FilePath -> FuseRet
flatOpenDirectory "/" = pure eOK
flatOpenDirectory _   = pure eNOENT

flatReadDirectory :: State -> FilePath -> FuseOut [(FilePath, FileStat)]
flatReadDirectory (State st) "/" = do
    ctx <- getFuseContext
    fs <- readIORef st
    ok $
        [ (".", dirStat ctx)
        , ("..", dirStat ctx)
        ] <>
        fmap (\(n, c) -> (n, fileStat c ctx)) fs
flatReadDirectory _ _ = fail eNOENT

flatOpen :: State -> FilePath -> OpenMode -> OpenFileFlags -> FuseOut Handle
flatOpen (State st) ('/':fileName) mode flags = do
    fs <- readIORef st
    case List.lookup fileName fs of
        Just f -> case mode of
            ReadOnly ->
                ok $ Handle mode
            WriteOnly -> case flags of
                OpenFileFlags{append = True} ->
                    ok $ Handle mode
                _ -> if BS.length f == 0
                    then ok $ Handle mode
                    else fail eACCES
            ReadWrite -> fail eACCES
        Nothing -> fail eNOENT
flatOpen _ _ _ _ = fail eNOENT

flatRead :: State -> FilePath -> Handle -> ByteCount -> FileOffset -> FuseOut ByteString
flatRead (State st) ('/':fileName) (Handle ReadOnly) bc fo = do
    fs <- readIORef st
    case List.lookup fileName fs of
        Nothing -> fail eNOENT
        Just f -> ok $ BS.take (fromIntegral bc) (BS.drop (fromIntegral fo) f)
flatRead _ _ _ _ _ = fail eNOENT

update :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
update a v = go
  where
    go [] = []
    go (x@(k, _):s)
        | a == k = (k, v):s
        | otherwise  = x:go s

flatWrite :: State -> FilePath -> Handle -> ByteString -> FileOffset -> FuseOut ByteCount
flatWrite (State st) ('/':fileName) (Handle WriteOnly) bs fo = do
    atomicModifyIORef' st $ \ fs ->
        case List.lookup fileName fs of
            Nothing -> (fs, Left eNOENT)
            Just f -> if fromIntegral (BS.length f) == fo
                then (update fileName (f <> bs) fs, Right . fromIntegral $ BS.length bs)
                else (fs, Left eACCES)
flatWrite _ _ (Handle ReadOnly) _ _ = fail eACCES
flatWrite _ _ _ _ _ = fail eNOENT

flatClose :: State -> FilePath -> Handle -> IO ()
flatClose _ _ (Handle _) = pure ()

flatCreateDevice :: State -> FilePath -> EntryType -> FileMode -> DeviceID -> FuseRet
flatCreateDevice (State st) ('/':fileName) _ _ _ = do
    atomicModifyIORef' st $ \ fs -> do
        case List.lookup fileName fs of
            Just _ -> (fs, eEXIST)
            Nothing -> ((fileName, ""):fs, eOK)
flatCreateDevice _ _ _ _ _ = pure eNOENT

flatFuseOps :: State -> FuseOperations Handle
flatFuseOps st = defaultFuseOps
    {
    -- fuseGetFileStat :: FilePath -> IO (Either Errno FileStat)
      fuseGetFileStat = flatGetFileStat st
    -- fuseReadSymbolicLink :: FilePath -> IO (Either Errno FilePath)
    -- fuseCreateDevice :: FilePath -> EntryType -> FileMode -> DeviceID -> IO Errno
    , fuseCreateDevice = flatCreateDevice st
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
    , fuseOpen = flatOpen st
    -- fuseRead :: FilePath -> fh -> ByteCount -> FileOffset -> IO (Either Errno ByteString)
    , fuseRead = flatRead st
    -- fuseWrite :: FilePath -> fh -> ByteString -> FileOffset -> IO (Either Errno ByteCount)
    , fuseWrite = flatWrite st
    -- fuseGetFileSystemStats :: String -> IO (Either Errno FileSystemStats)
    -- fuseFlush :: FilePath -> fh -> IO Errno
    -- fuseRelease :: FilePath -> fh -> IO ()
    , fuseRelease = flatClose st
    -- fuseSynchronizeFile :: FilePath -> SyncType -> IO Errno
    -- fuseOpenDirectory :: FilePath -> IO Errno
    , fuseOpenDirectory = flatOpenDirectory
    -- fuseReadDirectory :: FilePath -> IO (Either Errno [(FilePath, FileStat)])
    , fuseReadDirectory = flatReadDirectory st
    -- fuseReleaseDirectory :: FilePath -> IO Errno
    -- fuseSynchronizeDirectory :: FilePath -> SyncType -> IO Errno
    -- fuseAccess :: FilePath -> Int -> IO Errno
    -- fuseInit :: IO ()
    -- fuseDestroy :: IO ()
    }

main :: IO ()
main = do
    putStrLn "Starting"
    st <- mkState
    fuseMain (flatFuseOps st) defaultExceptionHandler
