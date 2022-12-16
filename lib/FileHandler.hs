module FileHandler where

import Datastructures
import JsonParser
import Text.Parsec
import System.IO

constructLevel :: String -> IO ()
constructLevel levelname = do
    withFile levelname ReadMode (\handle -> do
        contents <- hGetContents handle
        let parsed = (parse parseJSON "error" (contents))
        case parsed of
            Prelude.Left err -> error $ show err
            Prelude.Right json -> putStr (show json)
        )
