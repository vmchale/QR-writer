{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}

-- | Module providing several functions for creating QR codes and their signed counterparts
module Data.QRCodes (-- * Functions on objects
                      createQRCode
                    , createSecureQRCode
                    , createSecureQRCode'
                    -- * Functions for `ByteStrings`
                    , byteStringToQR
                    -- * functions to read QR codes
                    , readQRString
                    , readQRStrSec
                    , readQRStrSec'
                    ) where

import           Codec.Picture.Png          (writePng)
import           Control.Applicative        ((<$>))
import           Crypto.PubKey.RSA
import           Data.Binary
import qualified Data.ByteString.Char8      as BS
import           Data.ByteString.Lazy       (toStrict)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Char                  (toLower)
import           Data.Maybe
import           Data.QRCodes.Image
import           Data.QRCodes.Signature
import           Data.QRCodes.Utils
import           Data.Word                  (Word8)
import           System.Process

-- | Creates a signed QR code from a strict bytestring and path to keyfile/path.
-- If the keyfile does not already exist it will be generated, otherwise it will be read.
--
-- Note that QR codes may only contain a small number of characters, so encrypting can sometimes make an object too big to encode.
--
-- > byteStringToQRSec (BS.pack "hello") ".key.hk" "qrcode.png"
byteStringToQRSec :: BS.ByteString -> FilePath -> FilePath -> IO ()
byteStringToQRSec string keyfile filepath = flip byteStringToQR filepath =<< (fmap preserveUpper . flip mkSigFile keyfile) string

-- | Create a signed QR code from a strict `ByteString` and a key
--
-- > byteStringToQRSec' (BS.pack "Vanessa") (generate 256 0x10001)
byteStringToQRSec' :: BS.ByteString -> (PublicKey, PrivateKey) -> FilePath -> IO ()
byteStringToQRSec' string key filepath = flip byteStringToQR filepath =<< (fmap preserveUpper . flip mkSig key) string

-- | Creates a signed QR code from an object that is part of the ToJSON class
createSecureQRCode :: (Binary a) => a -> FilePath -> FilePath -> IO ()
createSecureQRCode object = byteStringToQRSec (toStrict $ encode object)

-- | Creates a signed QR code from an object that is part of the ToJSON class
createSecureQRCode' :: (Binary a) => a -> (PublicKey, PrivateKey) -> FilePath -> IO ()
createSecureQRCode' object = byteStringToQRSec' (toStrict $ encode object)

-- | Creates a QR code from an object that is part of the ToJSON class
--
-- > createQRCode userRecord "user-231.png"
createQRCode :: (Binary a) => a -> FilePath -> IO ()
createQRCode object filepath = let input = toStrict $ encode object in byteStringToQR input filepath

-- | Create a QR code, writing an image to the given 'FilePath'
byteStringToQR :: BS.ByteString -> FilePath -> IO ()
byteStringToQR input filepath = bsToImg input >>= writePng filepath

snd' (_, y, _) = y

-- | given a filepath, read the QR code as a string in all lowercase
--
-- > readQRString "picture.jpg"
readQRString :: FilePath -> IO String
readQRString filepath = map toLower . init . (drop 8 . snd') <$> readCreateProcessWithExitCode (shell $ "zbarimg " ++ filepath) ""

-- | given a filepath pointing to a QR code, get the contents & verify signature with the keyfile
--
-- > readQRStrSec "output.png" ".key.hk"
readQRStrSec :: (Binary a) => FilePath -> FilePath -> IO a
readQRStrSec filepath keyfile = decode . BSL.pack <$> do
    enc <- map toLower . init . drop 8 . snd' <$> readCreateProcessWithExitCode (shell $ "zbarimg " ++ filepath) ""
    fmap (liftEither BS.unpack) . flip checkSigFile keyfile . resolveUpper $ BS.pack enc

-- | Read an image containing a QR code, decode and verify the signature using the given key.
readQRStrSec' :: (Binary a) => FilePath -> (PublicKey, PrivateKey) -> IO a
readQRStrSec' filepath key = decode . BSL.pack <$> do
    enc <- map toLower . init . drop 8 . snd' <$> readCreateProcessWithExitCode (shell $ "zbarimg " ++ filepath) ""
    fmap (liftEither BS.unpack) . flip checkSig key . resolveUpper $ BS.pack enc
