{-#LANGUAGE OverloadedStrings #-}

import Data.QRCodes
import qualified Data.ByteString as B
import System.Environment (getArgs)

main :: IO ()
main = do
    pipeIn <- B.getContents
    filepath <- fmap (flip (!!) 0) getArgs
    byteStringToQRSec pipeIn filepath
    readQRStrSec filepath >>= print
