{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

import           Prelude hiding (takeWhile, FilePath, sequence)
import qualified Data.ByteString as B
import qualified Data.IntMap as M
import           Data.Text (Text)
import           Data.Text.Read (decimal, double)
import           Data.Time
import           Control.Monad.IO.Class (liftIO)

import           Data.Conduit
import qualified Data.Conduit.List   as CL
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Text   as CT
import           Data.Text.Encoding
import           Data.XML.Types
import           Filesystem.Path.CurrentOS ((</>), FilePath, encodeString)
import "xml-conduit" Text.XML.Stream.Parse


type Value = Either B.ByteString UTCTime


test :: IO ()
test = do
  let dir = "/home/tim/Dropbox/repo-job/xlsx-parser/data/my/"

  (Just sStrs) <- runResourceT $ parseSharedStrings dir $$ CL.head
  runResourceT $ parseWorksheet sStrs dir $$ CL.mapM_ print


------------------------------------------------------------------------------
-- SharedStrings
parseSharedStrings :: ResourceIO m => FilePath -> Source m (M.IntMap B.ByteString)
parseSharedStrings dir = parseFile def (dir </> "xl/sharedStrings.xml") $= conduitSharedStrings


conduitSharedStrings :: Resource m => Conduit Event m (M.IntMap B.ByteString)
conduitSharedStrings =
    conduitState (0, M.empty) push close
  where
    push state@(key, map) input =
      case parseShared input of
        Just val -> return $ StateProducing (key+1, M.insert key val map) []
        Nothing  -> return $ StateProducing state []
    close (key, map) = return [map]


parseShared :: Event -> Maybe B.ByteString
parseShared (EventContent c) = case c of
                                ContentText text -> Just $  encodeUtf8 text
                                ContentEntity _  -> Nothing
parseShared _ = Nothing


------------------------------------------------------------------------------
-- Worksheet
parseWorksheet :: ResourceIO m => M.IntMap B.ByteString -> FilePath -> Source m Value
parseWorksheet sStrs dir = parseFile def (dir </> "xl/worksheets/sheet1.xml") $= conduitWorksheet sStrs


conduitWorksheet :: Resource m => M.IntMap B.ByteString -> Conduit Event m Value
conduitWorksheet sStrs =
    conduitState ([], []) push close
  where
    push state@(as, ts) input = do
      case input of
        EventBeginElement name attrs -> if name == toName "c"
                                          then return $ StateProducing (attrs, ts) []
                                          else return $ StateProducing state []
        EventContent (ContentText t) -> if length as < 2
                                          then return $ StateProducing ([], (Left $ encodeUtf8 t) : ts) []
                                          else do
                                            let (name, content) = (as !! 1)
                                            case as !! 1 of
                                              ("s", [ContentText "1"]) -> do
                                                           let Right (val,_) = double t
                                                           return $ StateProducing ([], (Right . getTime $ val) : ts) []
                                              ("t", [ContentText "s"]) -> do
                                                           let Right (val,_) = decimal t
                                                           return $ StateProducing ([], (Left $ sStrs M.! val) : ts) []
                                              _ -> return $ StateProducing ([], (Left $ encodeUtf8 t) : ts) []
        _ -> return $ StateProducing state []

    close (_, ts) = return $ reverse ts

------------------------------------------------------------------------------
-- Other
toName :: Text -> Name
toName x = Name x (Just "http://schemas.openxmlformats.org/spreadsheetml/2006/main") Nothing


-- Parse date and time in the 1900 date base system.
-- ECMA-376, 3d edition:
-- "In the 1900 date base system, the lower limit is January 1, -9999
-- 00:00:00, which has serial value - 4346018. The upper-limit is
-- December 31, 9999, 23:59:59, which has serial value
-- 2,958,465.9999884.  The base date for this date base system is
-- December 30, 1899, which has a serial value of 0."
getTime :: Double -> UTCTime
getTime val = UTCTime day diff
  where
    days = truncate val
    day  = addDays days $ fromGregorian 1899 12 30

    time = val - (fromIntegral days)
    sec  = round $ time * 86400
    diff = secondsToDiffTime sec
