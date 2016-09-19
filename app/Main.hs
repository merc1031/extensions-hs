{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE MultiWayIf  #-}
{-# LANGUAGE PatternGuards  #-}
{-# LANGUAGE RecordWildCards  #-}
module Main where

import Control.Applicative ((<|>))
import Control.Monad (void, when)
import Data.Maybe (isJust, isNothing)
import GHC.Generics
import Options.Generic
import System.Exit (exitFailure)
import qualified Data.ByteString.Char8 as BSC

import Extensions

data AllArgs = AllArgs {
    file :: Maybe FilePath
  , folders :: [FilePath]
  , text :: Maybe BSC.ByteString
  , extensions :: [InHaskellPragma]
  , operation :: Maybe Operation
  , analysis :: Maybe Analysis
  } deriving (Generic)

newtype InHaskellPragma = InHaskellPragma { unInHaskellPragma :: HaskellPragma }
  deriving (Read)

instance ParseRecord AllArgs

instance ParseField InHaskellPragma
instance ParseFields Operation
instance ParseField Operation
instance ParseRecord Operation
instance ParseFields Analysis
instance ParseField Analysis
instance ParseRecord Analysis

main :: IO ()
main = getRecord "Pop" >>= \(args@(AllArgs {..})) -> do
  let exts = case (map unInHaskellPragma extensions) of
          [] -> defaultExtensions
          xs -> xs

  when (isJust operation && isJust analysis) $ do
    putStrLn "Cannot specify both operation and analyis"
    exitFailure

  when (isNothing operation && isNothing analysis) $ do
    putStrLn "Must specify operation or analysis"
    exitFailure

  go args exts
  where go (AllArgs {..}) exts
          | Just f <- file = if
                                | Just op <- operation -> changeExtensions op exts f
                                | Just an <- analysis -> analyze an exts f
          | fs <- folders
          , True <- [] /= fs = if
                                  | Just op <- operation -> changeMany op exts fs
                                  | Just an <- analysis -> analyzeMany an exts fs
          | Just content <- text = if
                                      | Just op <- operation -> do
                                          let res = changeContent op exts content
                                          print res
                                          return ()
                                      | Just an <- analysis -> do
                                          let res = analyzeContent an exts content
                                          print res
                                          return ()
          | otherwise = return ()
