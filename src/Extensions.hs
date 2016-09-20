{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Extensions where

import GHC.Generics
import Text.Megaparsec

import Control.Exception                    ( IOException
                                            , catch
                                            )
import Control.Monad                        ( forM
                                            , liftM
                                            , void
                                            )
import Data.Foldable                        ( foldrM )
import Data.List                            ( partition )
import Data.Maybe                           ( fromJust
                                            , catMaybes
                                            )
import Data.Monoid                          ( (<>)
                                            , mconcat
                                            )
import System.FilePath.Find                 ( find
                                            , filePath
                                            , (==?)
                                            , extension
                                            , always
                                            )
import System.FilePath.Manip                ( modifyInPlace )

import qualified Data.Map.Strict            as M
import qualified Data.Set                   as S
import qualified Data.ByteString.Char8      as BSC


import Str                                  ( str )

type Parser = Parsec Dec BSC.ByteString

data HaskellPragma = LanguagePragma BSC.ByteString
                   | LanguagePragmaList [BSC.ByteString]
                   | OtherPragma BSC.ByteString
                   deriving (Eq, Ord, Show, Read)

data Operation = RemoveFromFiles
               | AddToFiles
               deriving (Read, Show, Generic)

data Analysis = CountNonDefault
              | ExtensionCount
              deriving (Read, Show, Generic)

data ParsedHeader = ParsedHeader
  { pragmas :: [HaskellPragma]
  , rest :: BSC.ByteString
  }

exampleString :: BSC.ByteString
exampleString = [str|{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor, 
       DeriveAnyClass, MultiWayIf 
       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}
module X where
  more
  code
  here|]

exampleLanguagePragma :: BSC.ByteString
exampleLanguagePragma = [str|{-# LANGUAGE MultiParamTypeClasses #-}|]

examplePragma :: BSC.ByteString
examplePragma = [str|{-# OPTIONS_GHC -fdefer-type-errors #-}|]

exampleMultilineLanguagePragma :: BSC.ByteString
exampleMultilineLanguagePragma = [str|{-# LANGUAGE DeriveFunctor, 
       DeriveAnyClass, MultiWayIf 
       #-}|]

exampleMultiplePragma :: BSC.ByteString
exampleMultiplePragma = [str|{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}|]

exampleMultiplePragmaAndNewLines :: BSC.ByteString
exampleMultiplePragmaAndNewLines = [str|{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor, 
       DeriveAnyClass, MultiWayIf 
       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}|]

defaultExtensions :: [HaskellPragma]
defaultExtensions = [ LanguagePragma "OverloadedStrings"
                    , LanguagePragma "DeriveGeneric"
                    , LanguagePragma "FlexibleContexts"
                    , LanguagePragma "NamedFieldPuns"
                    , LanguagePragma "RecordWildCards"
                    , LanguagePragma "ScopedTypeVariables"
                    , LanguagePragma "MultiWayIf"
                    ]

analyzeContent :: Analysis
               -> [HaskellPragma]
               -> BSC.ByteString
               -> M.Map BSC.ByteString Int
analyzeContent an exts contents =
    let (l,rest) = BSC.breakSubstring "module" contents
    in case go l of
         Left _ -> M.singleton "No Analysis" 0
         Right r ->  r
    where go l = do
            ParsedHeader {..} <- parse parser "" l

            let (languagePragmas, otherPragmas) = partition (isLanguagePragma) pragmas
                fixedLanguagePragmas = concatMap fixLanguagePragma languagePragmas
            return $ case an of
                      CountNonDefault -> M.singleton ("Non Default") (length (S.toList $ (S.fromList fixedLanguagePragmas S.\\ S.fromList exts)))
                      ExtensionCount -> let uniqs = (S.toList $ (S.fromList fixedLanguagePragmas S.\\ S.fromList exts))
                                            prs = map ((,1)) $ catMaybes $ map unPragma uniqs
                                        in M.fromList prs
  
analyze :: (M.Map BSC.ByteString Int -> IO b)
        -> Analysis
        -> [HaskellPragma]
        -> FilePath
        -> IO b
analyze r an exts f = do
  putStrLn $ ("Analyzing: " ++ f)
  c <- BSC.readFile f
  let result = analyzeContent an exts c
  r result

foldrM' acc t f = foldrM f acc t

analyzeMany :: Analysis
            -> [HaskellPragma]
            -> [FilePath]
            -> IO ()
analyzeMany an exts paths = do
    filess <- forM paths $ find always (extension ==? ".hs")
    let files = concat filess
    case an of
      CountNonDefault -> 
        void $ forM files $ analyze (BSC.putStrLn . BSC.pack . show) an exts
      ExtensionCount -> do
        fr <- foldrM' M.empty files $ \file acc -> do
          res <- analyze (return) an exts file
          return $ M.unionWith ((+)) res acc
        BSC.putStrLn . BSC.pack . show $ fr

changeMany :: Operation
           -> [HaskellPragma]
           -> [FilePath]
           -> IO ()
changeMany op exts paths = do
    filess <- forM paths $ find always (extension ==? ".hs")
    let files = concat filess
    void $ forM files $ changeOneFile op exts

changeOneFile :: Operation
              -> [HaskellPragma]
              -> FilePath
              -> IO ()
changeOneFile = changeExtensions

modifyInPlace' :: FilePath
               -> (BSC.ByteString -> BSC.ByteString)
               -> IO ()
modifyInPlace' p a = do
  putStrLn $ ("Modifying: " ++ p)
  let d = flip modifyInPlace
  d p a `catch` \((e :: IOException)) -> putStrLn $ ("Error: " ++ show e)

changeExtensions :: Operation
                 -> [HaskellPragma]
                 -> FilePath
                 -> IO ()
changeExtensions op exts f = modifyInPlace' f (changeContent op exts)

changeContent :: Operation
              -> [HaskellPragma]
              -> BSC.ByteString
              -> BSC.ByteString
changeContent op exts contents =
    case attemptToChangeContents op exts contents of
        Left _ -> contents
        Right r -> r

attemptToChangeContents :: Operation
                        -> [HaskellPragma]
                        -> BSC.ByteString
                        -> Either (ParseError Char Text.Megaparsec.Dec) BSC.ByteString
attemptToChangeContents op exts contents =
    let (l,rest) = BSC.breakSubstring "module" contents
    in (\x -> return $ x <> rest) =<< (attemptToChangeContents' op exts l)

attemptToChangeContents' :: Operation
                         -> [HaskellPragma]
                         -> BSC.ByteString
                         -> Either (ParseError Char Text.Megaparsec.Dec) BSC.ByteString
attemptToChangeContents' op exts l = do
    ParsedHeader {..} <- parse parser "" l

    let (languagePragmas, otherPragmas) = partition (isLanguagePragma) pragmas
        fixedLanguagePragmas = concatMap fixLanguagePragma languagePragmas
    return $ case op of
      RemoveFromFiles -> (pragmasToBSC ((S.toList $ (S.fromList fixedLanguagePragmas S.\\ S.fromList exts)) <> otherPragmas)) <> rest
      AddToFiles -> (pragmasToBSC ((S.toList $ S.fromList (fixedLanguagePragmas <> exts)) <> otherPragmas)) <> rest

pragmasToBSC :: [HaskellPragma]
             -> BSC.ByteString
pragmasToBSC pragmas = mconcat $ catMaybes $ map (\p -> (flip BSC.snoc '\n') <$> unPragma p) pragmas

parser :: Parser ParsedHeader
parser = do
    space
    allPragma <- allPragmas
    rest <- BSC.pack <$> many anyChar
    space
    eof
    return $ ParsedHeader { pragmas = allPragma, rest = rest }

pragmaDirective :: BSC.ByteString
                -> BSC.ByteString
pragmaDirective l = "{-# " <> l <> " #-}"

unPragma :: HaskellPragma
         -> Maybe BSC.ByteString
unPragma (LanguagePragma b) = Just $ pragmaDirective ("LANGUAGE " <> b)
unPragma (LanguagePragmaList _) = Nothing
unPragma (OtherPragma b) = Just $ pragmaDirective b

isLanguagePragma :: HaskellPragma
                 -> Bool
isLanguagePragma (LanguagePragma _) = True
isLanguagePragma (LanguagePragmaList _) = True
isLanguagePragma _ = False

allPragmas :: Parser [HaskellPragma]
allPragmas = do
    many (apragma <* (optional newline))

apragma :: Parser HaskellPragma
apragma = do
    prag <- (try (languagePragmaList <?> "Language List")) <|> (try (languagePragma <?> "Language")) <|> (otherPragma <?> "Other")
    return prag


satisfyPragmaStart :: Parser ()
satisfyPragmaStart = bstring "{-#" *> space

satisfyPragmaEnd :: Parser ()
satisfyPragmaEnd = space <* bstring "#-}"

otherPragma :: Parser HaskellPragma
otherPragma = do
    prag <- between satisfyPragmaStart
                    satisfyPragmaEnd
                    (do
                        t <- many (letterChar <|> (oneOf ['_','-']))
                        space
                        v <- many (letterChar <|> digitChar <|> (oneOf ['_','-']) <|> spaceChar)
                        return $ t <> " " <> v)
    return $ OtherPragma $ BSC.pack prag

languagePragma :: Parser HaskellPragma
languagePragma = do
    prag <- between satisfyPragmaStart
                    satisfyPragmaEnd
                    (do bstring "LANGUAGE"
                        space
                        prag' <- many (letterChar <|> digitChar)
                        space
                        return prag'
                    )
    return $ LanguagePragma $ BSC.pack prag

bstring :: String
        -> Parser String
bstring = tokens (==)

languagePragmaList :: Parser HaskellPragma
languagePragmaList = do
    prag <- between (do satisfyPragmaStart
                        bstring "LANGUAGE"
                        space
                    )
                    satisfyPragmaEnd
                    (flip sepBy (char ',') $ do
                        space
                        inner <- many (letterChar <|> digitChar)
                        space
                        return $ BSC.pack inner
                    )
    return $ LanguagePragmaList prag

fixLanguagePragma :: HaskellPragma
                  -> [HaskellPragma]
fixLanguagePragma (LanguagePragmaList p) = map LanguagePragma p
fixLanguagePragma x = [x]

