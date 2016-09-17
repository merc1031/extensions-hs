{-# LANGUAGE OverloadedStrings #-}
module Extensions where
import System.FilePath.Find (find, filePath, (==?), extension, always)
import System.FilePath.Manip (modifyInPlace)

import Control.Monad (forM, liftM, void)
import Data.Maybe (fromJust, catMaybes)
import Data.Monoid ((<>), mconcat)
import Data.List (partition)
import qualified Data.Set as S


import qualified Data.ByteString.Char8 as BSC
import Text.Megaparsec
--import Text.Megaparsec.ByteString

type Parser = Parsec Dec BSC.ByteString

defaultExtensions :: [Pragma]
defaultExtensions = [ LanguagePragma "OverloadedStrings"
                    , LanguagePragma "DeriveGeneric"
                    , LanguagePragma "FlexibleContexts"
                    , LanguagePragma "NamedFieldPuns"
                    , LanguagePragma "RecordWildCards"
                    , LanguagePragma "ScopedTypeVariables"
                    , LanguagePragma "MultiWayIf"
                    ]

changeMany :: [Pragma] -> [FilePath] -> IO ()
changeMany exts paths = do
    filess <- forM paths $ find always (extension ==? ".hs")
    let files = concat filess
    void $ forM files $ changeOneFile exts

changeOneFile :: [Pragma] -> FilePath -> IO ()
changeOneFile = changeExtensions

modifyInPlace' :: FilePath -> (BSC.ByteString -> BSC.ByteString) -> IO ()
modifyInPlace' = flip modifyInPlace

changeExtensions :: [Pragma] -> FilePath -> IO ()
changeExtensions exts f = do
  modifyInPlace' f (changeContent exts)

changeContent :: [Pragma] -> BSC.ByteString -> BSC.ByteString
changeContent exts contents = case parseIt of
                                Left _ -> contents
                                Right r -> r
  where
    --Either (ParseError Char Dec) BSC.ByteString
    parseIt = do
      let (l,rest) = BSC.breakSubstring "module" contents
      pragmas <- parse parser "" l

      let (languagePragmas, otherPragmas) = partition (isLanguagePragma) pragmas
          fixedLanguagePragmas = concatMap fixLanguagePragma languagePragmas
      return $ pragmasToBSC ((S.toList $ S.fromList fixedLanguagePragmas) <> otherPragmas)

pragmasToBSC :: [Pragma] -> BSC.ByteString -- snoc
pragmasToBSC pragmas = mconcat $ catMaybes $ map (\p -> (\x -> x <> "\n") <$> unPragma p) pragmas

parser :: Parser [Pragma]
parser = sepBy apragma newline

data Pragma = LanguagePragma BSC.ByteString
            | LanguagePragmaList [BSC.ByteString]
            | OtherPragma BSC.ByteString
            deriving (Eq, Ord)

unPragma (LanguagePragma b) = Just b
unPragma (LanguagePragmaList _) = Nothing
unPragma (OtherPragma b) = Just b

isLanguagePragma :: Pragma -> Bool
isLanguagePragma (LanguagePragma _) = True
isLanguagePragma (LanguagePragmaList _) = True
isLanguagePragma _ = False

apragma :: Parser Pragma
apragma = do
  bstring "{-#"
  space
  prag <- languagePragmaList <|> languagePragma <|> (liftM (OtherPragma . BSC.pack) $ many letterChar)
  space
  bstring "#-}"
  return prag

languagePragma :: Parser Pragma
languagePragma = do
  bstring "LANGUAGE"
  space
  prag <- many letterChar
  return $ LanguagePragma $ BSC.pack prag

bstring = tokens (==)

languagePragmaList :: Parser Pragma
languagePragmaList = do
  bstring "LANGUAGE"
  space
  prag <- flip sepBy (char ',') $ do
    space
    inner <- many letterChar
    space
    return $ BSC.pack inner
  return $ LanguagePragmaList prag

fixLanguagePragma :: Pragma -> [Pragma]
fixLanguagePragma (LanguagePragmaList p) = map LanguagePragma p
fixLanguagePragma x = [x]

