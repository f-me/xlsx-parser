import System.Environment (getArgs)

import Data.Enumerator as E
import Data.Enumerator.Binary as B
import Data.Enumerator.Text as T

main = do
    f:_ <- getArgs

    let
        enumFileUtf8 = B.enumFile f $= T.decode utf8
        countChars = T.fold (\n _ -> n+1) 0
        countLines = T.fold (\n c -> if c == '\n' then n + 1 else n) 1

    count <- E.run_ (enumFileUtf8 $$ countChars)
    print count

    countLines <- E.run_ (enumFileUtf8 $$ countLines)
    print countLines
