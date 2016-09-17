module Main where

import Extensions

main :: IO ()
main = do
  changeExtensions defaultExtensions "testfile.hs"
  return ()
