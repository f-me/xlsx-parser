{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

import Control.Applicative
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import qualified Data.IntMap as M
import Data.List
import Data.Time
import System.Environment (getArgs)

import Codec.Archive.Zip
import Data.ByteString.Nums.Careless.Int
import Text.HTML.TagSoup


data WorkBook = WorkBook
    { sharedStrings :: M.IntMap B.ByteString
    , workSheets :: [WorkSheet]
    } deriving Show

data WorkSheet =  WorkSheet
    { cells :: [[Cell]]
    } deriving Show

data Cell = Cell
    { cellName  :: !B.ByteString
    , cellValue :: Value
    -- style?
    } deriving Show

data Value = Shared Int
           | Value B.ByteString
           | Time UTCTime
             deriving Show

parseXslx :: L.ByteString -> WorkBook
parseXslx xsl = WorkBook {sharedStrings = sStrs, workSheets = map work sheets}
  where
    ar =  toArchive xsl

    sStrs = case findEntryByPath "xl/sharedStrings.xml" ar of
              Nothing -> error "shared strings not found"
              Just xml -> parseSharedStrings $ fromEntry xml

    sheets = sort $ filter (isPrefixOf "xl/worksheets") $ filesInArchive ar

    work w = case findEntryByPath w ar of
               Nothing -> error "worksheet not found"
               Just xml -> parseWorksheet $ fromEntry xml


parseXslx' :: L.ByteString -> ([B.ByteString], Int) -> WorkBook
parseXslx' xls (cols, start) =
    WorkBook {sharedStrings = sStrs, workSheets = map work sheets}
  where
    wb = parseXslx xls

    sStrs = sharedStrings wb

    sheets = workSheets wb

    work sheet = WorkSheet $ map filterCells $ drop start $ cells sheet

    filterCells cs = filter (\c -> (cellName c) `elem` cols) cs

    elem name cols = any (`B.isPrefixOf` name) cols


parseSharedStrings :: L.ByteString -> M.IntMap B.ByteString
parseSharedStrings xml =
    M.fromAscList $ zip -- FIXME: to array
    [0..]
    [B.concat $ L.toChunks x | TagText x <- parseTags xml]


parseWorksheet :: L.ByteString -> WorkSheet
parseWorksheet xml = WorkSheet $ loop [] $ parseTags xml
  where
    loop row (TagOpen "c" attrs : TagOpen "v" _ : TagText val : rest) =
        loop (foldl' updCell undefCell attrs : row) rest
      where
        undefCell = Cell {cellName = "undefined", cellValue = Value . B.concat $ L.toChunks val}
        updCell c ("r", r) = c {cellName = B.concat $ L.toChunks r}
        updCell c ("t","s") = c {cellValue = Shared $ int val}
        updCell c ("s","1") = c {cellValue = Time . getTime . C.unpack . B.concat $ L.toChunks val}
        updCell c _ = c
    loop row (TagClose "row" : rest) =  reverse row : loop [] rest
    loop row (_:rest) = loop row rest
    loop _ [] = []

-- Parse date and time in the 1900 date base system.
-- ECMA-376, 3d edition:
-- "In the 1900 date base system, the lower limit is January 1, -9999
-- 00:00:00, which has serial value - 4346018. The upper-limit is
-- December 31, 9999, 23:59:59, which has serial value
-- 2,958,465.9999884.  The base date for this date base system is
-- December 30, 1899, which has a serial value of 0."
getTime :: String -> UTCTime
getTime s = UTCTime day diff
  where
    val  = read s

    days = truncate val
    day  = addDays days $ fromGregorian 1899 12 30

    time = val - (fromIntegral days)
    sec  = round $ time * 86400
    diff = secondsToDiffTime sec

main = do
    f:_ <- getArgs
    wb <- parseXslx <$> L.readFile f
    print $ M.size $ sharedStrings wb
    print $ foldl' (+) 0 $ map B.length $ M.elems $ sharedStrings wb
--    print $ foldl' (+) 0 $ map length $ concatMap cells $ workSheets wb

test f (cols, start) = do
    xlsx <- L.readFile f
    print . len $ parseXslx' xlsx (map C.pack cols, start)
  where
    len wb = foldl' (+) 0 $ map (length . concat . cells) $ workSheets wb
