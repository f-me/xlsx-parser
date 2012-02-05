{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString as B
import Data.ByteString.Lazy as BL
import System.Environment (getArgs)

import Data.Enumerator as E
import Data.Enumerator.Text as ET
import Data.Enumerator.Binary as EB
import Text.HTML.TagSoup

data Res = Res
    { cells :: [B.ByteString]
    } deriving Show

parse :: B.ByteString -> Res
parse xml = Res {cells = loop $ parseTags xml}
  where
    loop (TagOpen "c" _ : TagText "\n" :TagOpen "v" _ : TagText val : rest) = val : loop rest
    loop (t:rest) = loop rest
    loop [] = []

main = do
    f:_ <- getArgs
    test f

test path = do
    res <- E.run_ (EB.enumFile path $$ EB.head)
    print res
