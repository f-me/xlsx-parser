
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Xlsx.Parser where

import Control.Applicative
import Control.Monad.IO.Class
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.ByteString.Lazy as L

import qualified Data.IntMap as M

import qualified Codec.Archive.Zip as Zip

import Data.Conduit
import Data.XML.Types
import qualified Data.Conduit.List as CL
import qualified Text.XML.Stream.Parse as Xml


data Xlsx
  = Xlsx
    {archive :: Zip.Archive
--    ,sheets :: [FilePath]
--     ,sharedStrings :: Maybe FilePath
    }

data Columns
  = AllColumns
  | Columns [String]

data Cell
  = Cell {cellId :: Text, style :: Int, value :: Maybe Text}
  deriving Show


data Value
  = Str Text


-- parseToList :: FilePath -> [Column] -> [[Value]]
{-
parseConduit
  :: (MonadIO m, Resource m)
  => FilePath
  -> Int -- sheet number
  -> [Column]
  -> Source m Value
-}

xlsx fname = do
  ar <- Zip.toArchive <$> L.readFile fname
  return $ Xlsx
    {archive = ar
    }

parseSheet x@(Xlsx{..}) sheetId columns
  | sheetId < 0 || sheetId >= length sheets
    = error "parseSheet: Invalid sheetId"
  | otherwise
    = case xmlSource x (sheets !! sheetId) of
      Nothing -> error "An impossible happened"
      Just xml -> xml $= mkXmlCond getCell $$ CL.consume
  where
    sheets = sort
      $ filter (isPrefixOf "xl/worksheets")
      $ Zip.filesInArchive archive


getCell = Xml.tagName (n"c") cAttrs cParser
  where
    cAttrs = do
      cellId  <- Xml.requireAttr  "r"
      style   <- Xml.requireAttr  "s"
      sharing <- Xml.optionalAttr "t"
      Xml.ignoreAttrs
      return (cellId,style,sharing)

    cParser a@(cellId,style,sharing) = do
      val <- case sharing of
          Just "inlineStr" -> tagSeq ["is", "t"]
          Just "s" -> tagSeq ["v"] >> return (Just "shared str")
          Nothing  -> tagSeq ["v"]
      return $ Cell cellId (int style) val

n x = Name
  {nameLocalName = x
  ,nameNamespace = Just "http://schemas.openxmlformats.org/spreadsheetml/2006/main"
  ,namePrefix = Nothing}

tagSeq (x:xs)
  = Xml.tagNoAttr (n x)
  $ foldr (\x -> Xml.force "" . Xml.tagNoAttr (n x)) Xml.content xs


-- | Parse decimal from Text
int = either error fst . T.decimal 


xmlSource (Xlsx{..}) fname
  =   Xml.parseLBS Xml.def
  .   Zip.fromEntry
  <$> Zip.findEntryByPath fname archive


-- Get shared strings (if there are some) into IntMap
parseSharedStrings
  :: ResourceThrow m
  => Xlsx -> ResourceT m (M.IntMap Text)
parseSharedStrings x
  = case xmlSource x "xl/sharedStrings.xml" of
    Nothing -> return M.empty
    Just xml -> (M.fromAscList . zip [0..]) <$> xmlToText xml


-- | Fetch all text from xml
xmlToText xml = xml $= (mkXmlCond Xml.contentMaybe) $$ CL.consume

-- | Create conduit from xml sink
-- Resulting conduit filters nodes that `f` can consume and skips everything
-- else.
mkXmlCond f = sequenceSink () $ const
  $ CL.peek >>= maybe          -- try get current event form the stream
    (return Stop)              -- stop if stream is empty
    (\_ -> f >>= maybe         -- try consume current event
      (CL.drop 1 >> return (Emit () [])) -- skip it if can't process
      (return . Emit () . (:[])))        -- return result otherwise


