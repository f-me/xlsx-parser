{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import           Data.List
import           Data.Maybe
import           Data.Word (Word8)
import           System.Environment (getArgs)

import qualified Codec.Archive.Zip as Z
import qualified Codec.Archive.LibZip as LZ
import           Text.XML.Expat.SAX


-- LibZip and zip-archive comparison from the author of LibZip:
-- http://nix-tips.blogspot.com/2010/09/libzip-01-read-and-write-zip-archives.html

parse1 :: L.ByteString -> [B.ByteString]
parse1 input = [x | CharacterData x <- (parse defaultParseOptions input) :: [SAXEvent B.ByteString B.ByteString]]

main = do
  f:_ <- getArgs
  libZip f
--  zipArchive f
  return ()

zipArchive :: FilePath -> IO ()
zipArchive path = do
  file <- L.readFile path
  res $ Z.fromEntry $ fromJust $ Z.findEntryByPath "xl/worksheets/sheet1.xml" $ Z.toArchive file

libZip :: FilePath -> IO ()
libZip path = LZ.withArchive [] path $ do
                ws <- LZ.fileContents [] "xl/worksheets/sheet1.xml"
                liftIO $ print $ length (ws :: [Char])

test :: FilePath -> IO ()
test path = do
  file <- L.readFile path
  res file

res :: L.ByteString -> IO ()
res = print . length . parse1
