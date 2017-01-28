{-# LANGUAGE GADTs            #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Module providing several functions for creating QR codes and their signed counterparts
module Data.QRCodes (-- * Functions on objects
                      createQRCode
                    , createSecureQRCode
                    , createSecureQRCode'
                    -- * Functions for `ByteStrings`
                    , byteStringToQR
                    , byteStringToQRSec
                    , byteStringToQRSec'
                    -- * functions to read QR codes
                    , readQRString
                    , readQRStrSec
                    , readQRStrSec'
                    ) where

import Data.Aeson
import Codec.Picture.Png (writePng)
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Char8 as BS
import Data.Char (toLower)
import Control.Lens.Tuple
import Control.Lens (view)
import Control.Applicative ((<$>))
import System.Process
import Data.QRCodes.Utils
import Data.QRCodes.Signature
import Data.QRCodes.Image
import Data.Word (Word8)
import Crypto.PubKey.RSA

-- | Creates a signed QR code from a strict bytestring and path to keyfile/path.
-- If the keyfile does not already exist it will be generated, otherwise it will be read.
--
-- Note that QR codes may only contain a small number of characters, so encrypting can sometimes make an object too big to encode.
--
-- > byteStringToQRSec (BS.pack "hello") ".key.hk" "qrcode.png"
byteStringToQRSec :: BS.ByteString -> FilePath -> FilePath -> IO ()
byteStringToQRSec string keyfile filepath = (flip byteStringToQR filepath) =<< (((fmap preserveUpper) . (flip mkSigFile keyfile)) string)

-- | Create a signed QR code from a strict `ByteString` and a key
--
-- > byteStringToQRSec' (BS.pack "Vanessa") (generate 256 0x10001)
byteStringToQRSec' :: BS.ByteString -> (PublicKey, PrivateKey) -> FilePath -> IO ()
byteStringToQRSec' string key filepath = (flip byteStringToQR filepath) =<< (((fmap preserveUpper) . (flip mkSig key)) string)

-- | Creates a signed QR code from an object that is part of the ToJSON class
createSecureQRCode :: (ToJSON a) => a -> FilePath -> FilePath -> IO ()
createSecureQRCode object = byteStringToQRSec (toStrict $ encode object)

-- | Creates a signed QR code from an object that is part of the ToJSON class
createSecureQRCode' :: (ToJSON a) => a -> (PublicKey, PrivateKey) -> FilePath -> IO ()
createSecureQRCode' object = byteStringToQRSec' (toStrict $ encode object)

-- | Creates a QR code from an object that is part of the ToJSON class
--
-- > createQRCode userRecord "user-231.png"
createQRCode :: (ToJSON a) => a -> FilePath -> IO ()
createQRCode object filepath = let input = toStrict $ encode object in byteStringToQR input filepath

-- | Creates a QR code from a strict bytestring
byteStringToQR :: BS.ByteString -> FilePath -> IO ()
byteStringToQR input filepath = (bsToImg input) >>= writePng filepath

-- | given a filepath, read the QR code as a string in all lowercase
--
-- > readQRString "picture.jpg"
readQRString :: FilePath -> IO String
readQRString filepath = (map toLower) . init . (drop 8 . view _2) <$> readCreateProcessWithExitCode (shell $ "zbarimg " ++ filepath) ""

-- | given a filepath pointing to a QR code, get the contents & verify signature with the keyfile
--
-- > readQRStrSec "output.png" ".key.hk"
readQRStrSec :: FilePath -> FilePath -> IO String
readQRStrSec filepath keyfile = do
    enc <- (map toLower) . init . (drop 8) . (view _2) <$> readCreateProcessWithExitCode (shell $ "zbarimg " ++ filepath) ""
    (fmap $ liftEither BS.unpack) . (flip checkSigFile keyfile) . resolveUpper $ (BS.pack) enc

-- | Read an image containing a QR code, decode and verify the signature using the given key.
readQRStrSec' :: FilePath -> (PublicKey, PrivateKey) -> IO String
readQRStrSec' filepath key = do
    enc <- (map toLower) . init . (drop 8) . (view _2) <$> readCreateProcessWithExitCode (shell $ "zbarimg " ++ filepath) ""
    (fmap $ liftEither BS.unpack) . (flip checkSig key) . resolveUpper $ (BS.pack) enc
