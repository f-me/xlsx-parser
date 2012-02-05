{-# LANGUAGE OverloadedStrings #-}

import Text.XML.Stream.Parse
import System.Environment (getArgs)

main = do
    f:_ <- getArgs
    test f

--test path = undefined
test path = do
    vars <- parseFile_ def path $ force "error1" parseData
    print . length $ vars

parseAll = tagName "worksheet" ignoreAttrs $ \_ -> parseData

parseData = tagNoAttr "sheetData" $ do
              many parseC

parseC = tagName "c" ignoreAttrs $ \_ -> force "error2" parseV

parseV = tagNoAttr "v" $ do
           var <- content
           return var
