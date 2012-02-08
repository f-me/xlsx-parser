
import Control.Monad.IO.Class (liftIO)
import System.Environment (getArgs)

import Codec.Archive.LibZip

main = do
    f:_ <- getArgs

    withArchive [] f $ do
         bs <- fileContents [] "xl/worksheets/sheet1.xml"
         liftIO $ print $ length (bs :: [Char])
