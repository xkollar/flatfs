{-# LANGUAGE OverloadedStrings #-}
module Opts
    ( Config(..)
    , opts
    )
  where

import Control.Applicative ((<*>), (<**>), optional)
import Data.Functor ((<$>))
import Data.Maybe (Maybe)
import Data.Monoid ((<>))
import Data.Word (Word64)
import System.IO (FilePath)
import Text.Show (Show)

import Options.Applicative
    ( Parser, ParserInfo
    , argument, auto, fullDesc, help, helper, info, long, metavar, option, showDefault, str, strOption, value
    )
import Data.UUID (UUID)

import Lib(BlockSize(Size512), Label)


data Config = Config
    { blocks :: Maybe Word64
    , blockSize :: BlockSize
    , superblockSize :: BlockSize
    , label :: Label
    , uuid :: Maybe UUID
    , device :: FilePath
    }
  deriving Show

config :: Parser Config
config = Config
    <$> optional (option auto
        ( long "blocks"
        <> help "Number of blocks to use"
        <> metavar "BLOCKS"
        ))
    <*> option auto
        ( long "block-size"
        <> help "Block size"
        <> showDefault
        <> value Size512
        <> metavar "BLOCK_SIZE"
        )
    <*> option auto
        ( long "super-block-size"
        <> help "Size of superblock, >= block size"
        <> showDefault
        <> value Size512
        <> metavar "SUPER_BLOCK_SIZE"
        )
    <*> strOption
        ( long "label"
        <> help "Label, up to 16 characters"
        <> metavar "LABEL"
        <> value ""
        )
    <*> optional (option auto
        ( long "uuid"
        <> help "UUID"
        <> metavar "UUID"
        ))
    <*> argument str (metavar "DEVICE")

opts :: ParserInfo Config
opts = info (config <**> helper) fullDesc
