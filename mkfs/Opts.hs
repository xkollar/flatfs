module Opts
    ( Config(..)
    , opts
    )
  where

import Control.Applicative ((<*>), (<**>))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(Nothing))
import Data.Monoid ((<>))
import Data.Word (Word64)
import System.IO (FilePath)
import Text.Show (Show)

import Options.Applicative
    ( Parser, ParserInfo
    , argument, auto, fullDesc, help, helper, info, long, metavar, option, showDefault, str, value
    )

import Lib(BlockSize(Size512))


data Config = Config
    { blocks :: Maybe Word64
    , blockSize :: BlockSize
    , superblockSize :: BlockSize
    , device :: FilePath
    }
  deriving Show

config :: Parser Config
config = Config
    <$> option auto
        ( long "blocks"
        <> help "Number of blocks to use"
        <> metavar "BLOCKS"
        <> value Nothing
        )
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
    <*> argument str (metavar "DEVICE")

opts :: ParserInfo Config
opts = info (config <**> helper) fullDesc
