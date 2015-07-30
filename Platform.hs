{-# LANGUAGE CPP #-}

module Platform where

#ifdef OS_Linux

import System.Posix

isInputInteractive :: IO Bool
isInputInteractive = queryTerminal stdInput

isOutputInteractive :: IO Bool
isOutputInteractive = queryTerminal stdOutput

#else

isInputInteractive :: IO Bool
isInputInteractive = return True

isOutputInteractive :: IO Bool
isOutputInteractive = return True

#endif
