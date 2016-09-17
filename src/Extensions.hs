{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Extensions where

import Control.Monad (forM, liftM, void)
import Data.List (partition)
import Data.Maybe (fromJust, catMaybes)
import Data.Monoid ((<>), mconcat)
import System.FilePath.Find (find, filePath, (==?), extension, always)
import System.FilePath.Manip (modifyInPlace)
import Text.Megaparsec

import Str (str)

import qualified Data.Set as S
import qualified Data.ByteString.Char8 as BSC

type Parser = Parsec Dec BSC.ByteString

data HaskellPragma = LanguagePragma BSC.ByteString
                   | LanguagePragmaList [BSC.ByteString]
                   | OtherPragma BSC.ByteString
                   deriving (Eq, Ord, Show, Read)


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

changeMany :: [HaskellPragma] -> [FilePath] -> IO ()
changeMany exts paths = do
    filess <- forM paths $ find always (extension ==? ".hs")
    let files = concat filess
    void $ forM files $ changeOneFile exts

changeOneFile :: [HaskellPragma] -> FilePath -> IO ()
changeOneFile = changeExtensions

modifyInPlace' :: FilePath -> (BSC.ByteString -> BSC.ByteString) -> IO ()
modifyInPlace' = flip modifyInPlace

changeExtensions :: [HaskellPragma] -> FilePath -> IO ()
changeExtensions exts f = modifyInPlace' f (changeContent exts)

changeContent :: [HaskellPragma] -> BSC.ByteString -> BSC.ByteString
changeContent exts contents =
    case parseIt exts contents of
        Left _ -> contents
        Right r -> r

parseIt :: [HaskellPragma] -> BSC.ByteString -> Either (ParseError Char Text.Megaparsec.Dec) BSC.ByteString
parseIt exts contents =
    let (l,rest) = BSC.breakSubstring "module" contents
    in (\x -> return $ x <> rest) =<< (parseInner exts l)

parseInner :: [HaskellPragma] -> BSC.ByteString -> Either (ParseError Char Text.Megaparsec.Dec) BSC.ByteString
parseInner exts l = do
    pragmas <- parse parser "" l

    let (languagePragmas, otherPragmas) = partition (isLanguagePragma) pragmas
        fixedLanguagePragmas = concatMap fixLanguagePragma languagePragmas
    return $ pragmasToBSC ((S.toList $ S.fromList (fixedLanguagePragmas <> exts)) <> otherPragmas)

pragmasToBSC :: [HaskellPragma] -> BSC.ByteString -- snoc
pragmasToBSC pragmas = mconcat $ catMaybes $ map (\p -> (\x -> x <> "\n") <$> unPragma p) pragmas

parser :: Parser [HaskellPragma]
parser = do
    space
    allPragma <- allPragmas
    space
    eof
    return allPragma

unPragma :: HaskellPragma -> Maybe BSC.ByteString
unPragma (LanguagePragma b) = Just b
unPragma (LanguagePragmaList _) = Nothing
unPragma (OtherPragma b) = Just b

isLanguagePragma :: HaskellPragma -> Bool
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
                        v <- many (letterChar <|> digitChar <|> (oneOf ['_','-']))
                        return $ t <> v)
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

bstring :: String -> Parser String
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

fixLanguagePragma :: HaskellPragma -> [HaskellPragma]
fixLanguagePragma (LanguagePragmaList p) = map LanguagePragma p
fixLanguagePragma x = [x]

