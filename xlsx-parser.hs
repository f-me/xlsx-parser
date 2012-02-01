{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import qualified Data.IntMap as M
import Data.List
import System.Environment (getArgs)

import Codec.Archive.Zip
import Data.ByteString.Nums.Careless.Int
import Text.HTML.TagSoup

import Data.Array.Unboxed
import Foreign.StablePtr


data WorkBook = WorkBook
    { sharedStrings :: M.IntMap B.ByteString
    , workSheets :: [WorkSheet]
    } deriving Show

data WorkSheet =  WorkSheet
    { cells :: [[Cell]]
    } deriving Show

data Cell = Cell
    { cellName  :: !B.ByteString
    , cellValue :: Either Int B.ByteString
    -- style?
    } deriving Show


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


type Arr = UArray Int (StablePtr B.ByteString)

mkArr :: Int -> [B.ByteString] -> IO Arr
mkArr sz xs = listArray (0,sz-1) <$> mapM newStablePtr xs

freeArr :: Arr -> IO ()
freeArr = mapM_ freeStablePtr . elems

arrGet :: Arr -> Int -> IO B.ByteString
arrGet a i = deRefStablePtr $ a ! i


parseWorksheet :: L.ByteString -> WorkSheet
parseWorksheet xml = WorkSheet $ loop [] $ parseTags xml
  where
    loop row (TagOpen "c" attrs : TagOpen "v" _ : TagText val : rest) =
        loop (foldl' updCell undefCell attrs : row) rest
      where
        undefCell = Cell {cellName = "undefined", cellValue = Right $ B.concat $ L.toChunks val}
        updCell c ("r", r) = c {cellName = B.concat $ L.toChunks r}
        updCell c ("t","s") = c {cellValue = Left $ int val}
        updCell c _ = c
    loop row (TagClose "row" : rest) =  reverse row : loop [] rest
    loop row (_:rest) = loop row rest
    loop _ [] = []


main = do
    f:_ <- getArgs
    wb <- parseXslx <$> L.readFile f
    print $ M.size $ sharedStrings wb
    print $ foldl' (+) 0 $ map B.length $ M.elems $ sharedStrings wb

test f (cols, start) = do
    xlsx <- L.readFile f
    print . len $ parseXslx' xlsx (map C.pack cols, start)
  where
    len wb = foldl' (+) 0 $ map (length . concat . cells) $ workSheets wb
