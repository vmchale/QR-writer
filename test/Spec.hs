{-# LANGUAGE OverloadedStrings #-} 

import Data.QRCodes
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Data.QRCodes" $ do
    it "writes a QR code" $ do
      byteStringToQR "hello world" "qrcode.png" >>= (`shouldBe` ())
    it "reads a QR code" $ do
      readQRString "qrcode.png" >>= (`shouldBe` "hello world")
    it "writes a secure QR code" $ do
      byteStringToQRSec "helloworld" ".key.hk" "qrcode-sec.png" >>= (`shouldBe` ())
    it "reads a secire QR code" $ do
      readQRStrSec "qrcode-sec.png" ".key.hk" >>= (`shouldBe` "helloworld")
