{-# LANGUAGE CPP #-}

module Platform where

#ifdef OS_Linux

import System.Posix

interactiveInput :: IO Bool
interactiveInput = queryTerminal stdInput

interactiveOutput :: IO Bool
interactiveOutput = queryTerminal stdOutput

#else

interactiveInput :: IO Bool
interactiveInput = return True

interactiveOutput :: IO Bool
interactiveOutput = return True

#endif
