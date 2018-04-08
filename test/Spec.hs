{-# LANGUAGE OverloadedStrings #-}

import           Data.QRCodes
import           Test.Hspec

main :: IO ()
main = hspec $
  describe "Data.QRCodes" $ do
  it "writes a QR code" $
    byteStringToQR "hello world" "qrcode.png" >>= (`shouldBe` ())
  it "reads a QR code" $
    readQRString "qrcode.png" >>= (`shouldBe` "hello world")
  it "writes a secure QR code" $
    createSecureQRCode ("small" :: String) ".key.hk" "qrcode-sec.png" >>= (`shouldBe` ())
  it "reads a secure QR code" $
    (readQRStrSec "qrcode-sec.png" ".key.hk" :: IO String) >>= (`shouldBe` "small")
