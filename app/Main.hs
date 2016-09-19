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
  , operation :: Operation
  } deriving (Generic)

newtype InHaskellPragma = InHaskellPragma { unInHaskellPragma :: HaskellPragma }
  deriving (Read)

instance ParseRecord AllArgs

instance ParseField InHaskellPragma
instance ParseFields Operation
instance ParseField Operation
instance ParseRecord Operation

main :: IO ()
main = getRecord "Pop" >>= \args -> do
  let exts = case (map unInHaskellPragma $ extensions args) of
          [] -> defaultExtensions
          xs -> xs
  go (operation args) args exts
  where go op args exts
          | Just f <- file args = changeExtensions op exts f
          | fs <- folders args
          , True <- [] /= fs = changeMany op exts fs
          | Just content <- text args = do
                let res = changeContent op exts content
                print res
                return ()
          | otherwise = return ()
