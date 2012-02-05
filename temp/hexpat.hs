{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import           System.Environment (getArgs)

import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB
import           Text.XML.Expat.Tree
import           Text.XML.Expat.Proc
import           Text.XML.Expat.Enumerator

parse1 input =
    onlyText $ concatMap eChildren . concatMap (onlyElems . eChildren) $ filterElements (\n -> eName n == C.pack "c") xml
  where
    (xml, mErr) = parse defaultParseOptions input :: (UNode B.ByteString, Maybe XMLParseError)

parse2 input = undefined

enumXML path =
  E.joinE (EB.enumFile path) $ parseBytesIO Nothing


main = do
  f:_ <- getArgs
  test f


test :: FilePath -> IO ()
test path = do
  file <- L.readFile path
  print . length $ parse1 file
