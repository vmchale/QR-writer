{-# LANGUAGE OverloadedStrings #-} 

import Data.QRCodes

main :: IO ()
main = do
    byteStringToQRSec "hello friend" ".key.hk" "qrcode.png"
    --readQRStrSec "qrcode.png" ".key.hk" >>= print
