{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Data.List
--import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import System.Environment (getArgs)

import Text.HTML.TagSoup

import Debug.Trace

debug = flip trace


data Res = Res
    { cells :: [L.ByteString]
    } deriving Show


parse1 :: L.ByteString -> Res
parse1 xml = Res {cells = loop $ pt xml}
  where
    loop (TagOpen "c" _ : TagOpen "v" _ : TagText val : rest) = val : loop rest
    loop (t:rest) = loop rest
    loop [] = []


parse2 :: L.ByteString -> Res
parse2 xml = Res $ parse' $ pt xml
  where
    parse' = map parseItem . sections (~== ("<v>" :: String)) . dropWhile (~/= ("<v>" :: String))


parseItem xs = innerText a
  where
    (_,a) = break (~== ("<v>" :: String)) (takeWhile (~/= ("</v>" :: String)) xs)


pt = parseTags


main = do
  f:_ <- getArgs
  test f


test :: FilePath -> IO ()
test path = do
  file <- L.readFile path
  print . length . cells $ parse1 file

