
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import           Control.Monad (when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import           System.Environment (getArgs)

import           Codec.Archive.Zip
import qualified Data.Text as T
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB
import qualified Data.Enumerator.List as EL
import           Database.Enumerator
import           Database.Sqlite.Enumerator
import           Data.XML.Types
import           Text.XML.Expat.Tree
import           Text.XML.Expat.Proc
import           Text.XML.Expat.Enumerator

------------------------------------------------------------------------------
-- DB

actions :: Show a => [a] -> DBM mark Session ()
actions ss = do
  createDB
  insertShared ss
--  liftIO $ print $ "inserted: " ++ show i
--  liftIO $ print res

createDB :: DBM mark Session ()
createDB = do
    tables <- doQuery (sql "SELECT name FROM sqlite_master \
                           \WHERE type='table' \
                           \ORDER BY name")
                      getTables []

    when ("shared" `notElem` tables) $ do
        execDDL (sql "CREATE TABLE shared (\
                     \sharedId INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, \
                     \sharedVal TEXT NOT NULL)")
  where
    getTables :: Monad m => String -> IterAct m [String]
    getTables table accum = result' (table:accum)


insertShared :: Show a => [a] -> DBM mark Session ()
insertShared xs = do
    withTransaction Serialisable (do
        mapM_ insert xs
        )
  where
    insert x = execDML (cmdbind "INSERT INTO shared (sharedVal) VALUES (?)" [bindP x])

------------------------------------------------------------------------------
-- Parse

--enumXML :: FilePath -> E.Enumerator Event IO b
enumXML :: FilePath -> E.Enumerator Event IO b
enumXML path =
  E.joinE (EB.enumFile path) $ parseBytesIO Nothing

--parseLBS :: L.ByteString -> E.Iteratee Event IO a -> IO (Either SomeException a)
parseLBS lbs p =
    E.run $ enumSingle (L.toChunks lbs) E.$$ E.joinI
        $ parseBytesIO Nothing          E.$$ p

enumSingle as (E.Continue k) = k $ E.Chunks as
enumSingle _ step = E.returnI step

parseSharedStrings accum = do
    x <- EL.head
    case x of
      Just (EventContent c) -> case c of
                                 ContentText t -> do
--                                        liftIO $ print t
                                        parseSharedStrings (t:accum)
                                 _ -> parseSharedStrings accum
      Nothing -> return accum
      _ -> parseSharedStrings accum

------------------------------------------------------------------------------
-- Main

main = do
  f:_ <- getArgs
  test f

test :: FilePath -> IO ()
test path = do
    file <- L.readFile path
    (Right res) <- parseLBS (sStrs file) $ parseSharedStrings []
--    res <- E.run_ (enumXML path E.$$ parseSharedStrings [])
--    print res
    withSession (connect "takusen165") (actions res)
  where

sStrs :: L.ByteString -> L.ByteString
sStrs f = case findEntryByPath "xl/sharedStrings.xml" $ toArchive f of
              Nothing -> error "shared strings not found"
              Just xml -> fromEntry xml

myTest = withSession (connect "takusen165") (actions  [1..100])
