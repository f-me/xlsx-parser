{-# LANGUAGE OverloadedStrings #-}

import Text.XML
import Text.XML.Cursor
import System.Environment (getArgs)

main = do
    f:_ <- getArgs
    test f

test path = do
    doc <- readFile_ def path
    let
        cursor = fromDocument doc
        vars = cursor $/ element "c"
                      &/ element "v"
                      &// content

    print . length $ vars
