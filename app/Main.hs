{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE PatternGuards  #-}
module Main where

import Control.Applicative ((<|>))
import Control.Monad (void)
import GHC.Generics
import Options.Generic
import qualified Data.ByteString.Char8 as BSC

import Extensions

data AllArgs = AllArgs {
    file :: Maybe FilePath
  , folders :: [FilePath]
  , text :: Maybe BSC.ByteString
  , extensions :: [InHaskellPragma]
  } deriving (Generic)

newtype InHaskellPragma = InHaskellPragma { unInHaskellPragma :: HaskellPragma }
  deriving (Read)

instance ParseRecord AllArgs

instance ParseField InHaskellPragma

main :: IO ()
main = getRecord "Pop" >>= \args -> do
  let exts = case (map unInHaskellPragma $ extensions args) of
          [] -> defaultExtensions
          xs -> xs
  go args exts
  where go args exts
          | Just f <- file args = changeExtensions exts f
          | fs <- folders args
          , True <- [] /= fs = changeMany exts fs
          | Just content <- text args = do
                let res = changeContent exts content
                print res
                return ()
          | otherwise = return ()
