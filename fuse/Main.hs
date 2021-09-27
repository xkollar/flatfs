{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import GHC.Real (fromIntegral)

import Control.Applicative (pure)
import Control.Monad ((>>=))
import Data.Bool (Bool(True))
import Data.Either (Either(Left, Right))
import Data.Eq ((==))
import Data.Function (($), (.))
import Data.Functor ((<$>), fmap)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (foldr1)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid ((<>))
import System.IO (FilePath, IO, putStrLn)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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
type FS = Map FilePath File
newtype State = State (IORef FS)

data Handle = Handle OpenMode

type FuseOut a = IO (Either Errno a)
type FuseRet = IO Errno

fail :: Errno -> FuseOut a
fail = pure . Left

ok :: a -> FuseOut a
ok = pure . Right

mkState :: IO State
mkState = State <$> (newIORef . Map.fromList)
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
    case Map.lookup fileName fs of
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
        fmap (\(n, c) -> (n, fileStat c ctx)) (Map.assocs fs)
flatReadDirectory _ _ = fail eNOENT

flatOpen :: State -> FilePath -> OpenMode -> OpenFileFlags -> FuseOut Handle
flatOpen (State st) ('/':fileName) = \case
    ReadOnly -> \ _flags -> do
        fs <- readIORef st
        if Map.member fileName fs
            then ok $ Handle ReadOnly
            else fail eNOENT
    WriteOnly -> \case
        OpenFileFlags{append = True} -> do
            fs <- readIORef st
            if Map.member fileName fs
                then ok $ Handle WriteOnly
                else fail eNOENT
        _ -> do
            fs <- readIORef st
            case Map.lookup fileName fs of
                Nothing -> fail eNOENT
                Just f -> if BS.length f == 0
                    then ok $ Handle WriteOnly
                    else fail eACCES
    ReadWrite -> \ _flags -> fail eACCES
flatOpen _ _ = \ _ _ -> fail eNOENT

flatRead :: State -> FilePath -> Handle -> ByteCount -> FileOffset -> FuseOut ByteString
flatRead (State st) ('/':fileName) (Handle ReadOnly) bc fo = do
    fs <- readIORef st
    case Map.lookup fileName fs of
        Nothing -> fail eNOENT
        Just f -> ok $ BS.take (fromIntegral bc) (BS.drop (fromIntegral fo) f)
flatRead _ _ _ _ _ = fail eNOENT

flatWrite :: State -> FilePath -> Handle -> ByteString -> FileOffset -> FuseOut ByteCount
flatWrite (State st) ('/':fileName) (Handle WriteOnly) bs fo = do
    atomicModifyIORef' st $ \ fs ->
        case Map.lookup fileName fs of
            Nothing -> (fs, Left eNOENT)
            Just f -> if fromIntegral (BS.length f) == fo
                then (Map.insert fileName (f <> bs) fs, Right . fromIntegral $ BS.length bs)
                else (fs, Left eACCES)
flatWrite _ _ (Handle ReadOnly) _ _ = fail eACCES
flatWrite _ _ _ _ _ = fail eNOENT

flatClose :: State -> FilePath -> Handle -> IO ()
flatClose _ _ (Handle _) = pure ()

flatCreateDevice :: State -> FilePath -> EntryType -> FileMode -> DeviceID -> FuseRet
flatCreateDevice (State st) ('/':fileName) _ _ _ = do
    atomicModifyIORef' st $ \ fs -> do
        if Map.member fileName fs
            then (fs, eEXIST)
            else (Map.insert fileName "" fs, eOK)
flatCreateDevice _ _ _ _ _ = pure eNOENT

flatFuseOps :: State -> FuseOperations Handle
flatFuseOps st = defaultFuseOps
    { fuseGetFileStat = flatGetFileStat st
    , fuseOpenDirectory = flatOpenDirectory
    , fuseReadDirectory = flatReadDirectory st
    , fuseCreateDevice = flatCreateDevice st
    , fuseOpen = flatOpen st
    , fuseRead = flatRead st
    , fuseWrite = flatWrite st
    , fuseRelease = flatClose st
    }

main :: IO ()
main = do
    putStrLn "Starting"
    st <- mkState
    fuseMain (flatFuseOps st) defaultExceptionHandler
