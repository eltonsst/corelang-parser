{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import           Lib
import           System.IO                      ( IOMode(ReadMode)
                                                , hGetContents
                                                , openFile
                                                )

readF :: IO String
readF = do
  inh <- openFile "input.txt" ReadMode
  hGetContents inh

main :: IO ()
main = do
  inp <- readF
  print (parse parseProg inp)
