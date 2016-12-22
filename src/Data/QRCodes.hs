{-# LANGUAGE GADTs            #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Module providing several functions for creating QR codes and their signed counterparts
module Data.QRCodes ( createSecureQRCode
                    , createQRCode
                    , byteStringToQR
                    , byteStringToQRSec
                    , readQRString
                    , readQRStrSec
                    ) where

import Data.Aeson
import Data.QRCode
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

-- | Creates a signed QR code from a strict bytestring
byteStringToQRSec :: BS.ByteString -> FilePath -> IO ()
byteStringToQRSec string filepath = (flip byteStringToQR filepath) =<< (((fmap preserveUpper) . mkSig) string)

-- | Creates a signed QR code from an object that is part of the ToJSON class
createSecureQRCode :: (ToJSON a) => a -> FilePath -> IO ()
createSecureQRCode object = byteStringToQRSec (toStrict $ encode object)

-- | Creates a QR code from an object that is part of the ToJSON class
createQRCode :: (ToJSON a) => a -> FilePath -> IO ()
createQRCode object filepath = let input = toStrict $ encode object in byteStringToQR input filepath

-- | Creates a QR code from a strict bytestring
byteStringToQR :: BS.ByteString -> FilePath -> IO ()
byteStringToQR input filepath = do
    smallMatrix <- toMatrix <$> encodeByteString input Nothing QR_ECLEVEL_H QR_MODE_EIGHT False
    let qrMatrix = fattenList 8 $ map (fattenList 8) smallMatrix
    writePng filepath (encodePng qrMatrix)

-- | given a filepath, read the QR code as a string in all lowercase
readQRString :: FilePath -> IO String
readQRString filepath = (map toLower) . init . (drop 8 . view _2) <$> readCreateProcessWithExitCode (shell $ "zbarimg " ++ filepath) ""

-- | given a filepath pointing to a QR code, get the contents & verify signature
readQRStrSec :: FilePath -> IO String
readQRStrSec filepath = do
    enc <- (map toLower) . init . (drop 8) . (view _2) <$> readCreateProcessWithExitCode (shell $ "zbarimg " ++ filepath) ""
    (fmap $ liftEither show) . checkSig . resolveUpper $ (BS.pack) enc
