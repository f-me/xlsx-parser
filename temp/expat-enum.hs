{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import           System.Environment (getArgs)

import qualified Data.Text as T
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB
import qualified Data.Enumerator.List as EL
import           Data.XML.Types
import           Text.XML.Expat.Tree
import           Text.XML.Expat.Proc
import           Text.XML.Expat.Enumerator



enumXML :: FilePath -> E.Enumerator Event IO b
enumXML path =
  E.joinE (EB.enumFile path) $ parseBytesIO Nothing

parseRes :: (Num b, Monad m) => b -> E.Iteratee Event m b
parseRes count = do
    x <- EL.head
    case x of
      Just (EventContent c) -> case c of
                                 ContentText t -> do
--                                        liftIO $ print t
                                        parseRes (1+count)
                                 _ -> parseRes count
      Nothing -> return count
      _ -> parseRes count


main = do
  f:_ <- getArgs
  test f


test :: FilePath -> IO ()
test path = do
  res <- E.run_ (enumXML path E.$$ parseRes 0)
  print res
