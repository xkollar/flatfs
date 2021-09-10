module Main where

import Control.Applicative (pure)

import System.Fuse (FuseOperations, fuseMain, defaultFuseOps, defaultExceptionHandler)
import System.IO (IO)

data State = State

data Handle = Handle

mkState :: IO State
mkState = pure State

main :: IO ()
main = do
    st <- mkState
    fuseMain (flatFuseOps st) defaultExceptionHandler

flatFuseOps :: State -> FuseOperations Handle
flatFuseOps _ = defaultFuseOps
