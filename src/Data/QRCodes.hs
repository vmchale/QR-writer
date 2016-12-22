{-# LANGUAGE GADTs            #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Module providing several functions for creating QR codes and their signed counterparts
module Data.QRCodes --( createSecureQRCode
                    --, createQRCode
                    --, byteStringToQR
                    --, byteStringToQRSec
                    --, readQRStrSec
                    --) where
                    where

import Data.Aeson
import Data.QRCode
import Codec.Picture.Types as T
import Codec.Picture.Png (writePng)
import Data.Word (Word8)
import qualified Data.Vector.Storable as V
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import Data.List (replicate)
import Data.Char (toLower)
import Prelude as P
import Crypto.PubKey.RSA as Cr
import Jose.Jws
import System.Directory (doesFileExist)
import Control.Lens.Tuple
import Control.Lens (view)
import Jose.Jwt (unJwt, JwtError)
import Jose.Jwa (JwsAlg (RS256))
import Data.Either (either)
import Data.Bits ((.&.))
import Control.Applicative ((<$>))
import System.Process
import Data.Char (toLower, toUpper)
import Data.String.Utils (replace)

-- | Verify a signed token
checkSig :: BS.ByteString -> IO (Either JwtError BS.ByteString)
checkSig tok = do
    key <- read <$> readFile ".key.hk" :: IO (Cr.PublicKey, Cr.PrivateKey)
    let jws = rsaDecode (view _1 key) tok
    return $ fmap (view _2) jws

-- | Sign a token (note that we map the string to all lowercase before signing since QR codes do not distinguish case once read)
mkSig :: BS.ByteString -> IO BS.ByteString
mkSig string = do
    switch <- doesFileExist ".key.hk"
    if not switch then do
        putStrLn "generating key..."
        key <- Cr.generate 256 0x10001
        writeFile ".key.hk" (show key)
    else
        return ()
    key' <- read <$> readFile ".key.hk" :: IO (Cr.PublicKey, Cr.PrivateKey)
    signedToken <- rsaEncode RS256 (view _2 key') string
    let signed = fmap unJwt signedToken
    liftEither id (return <$> signed)

-- | Creates a signed QR code from a strict bytestring
byteStringToQRSec :: BS.ByteString -> FilePath -> IO ()
byteStringToQRSec string filepath = (flip byteStringToQR filepath) =<< (((fmap preserveUpper) . mkSig) string)

-- | Creates a signed QR code from an object that is part of the ToJSON class
createSecureQRCode :: (ToJSON a) => a -> FilePath -> IO ()
createSecureQRCode object = byteStringToQRSec (toStrict $ encode object)

liftEither :: (Show b, Monad m) => (t -> m a) -> Either b t -> m a
liftEither = either (error . show)

-- | Creates a QR code from an object that is part of the ToJSON class
createQRCode :: (ToJSON a) => a -> FilePath -> IO ()
createQRCode object filepath = let input = toStrict $ encode object in byteStringToQR input filepath

-- | Creates a QR code from a strict bytestring
byteStringToQR :: BS.ByteString -> FilePath -> IO ()
byteStringToQR input filepath = do
    smallMatrix <- toMatrix <$> encodeByteString input Nothing QR_ECLEVEL_H QR_MODE_EIGHT False
    let qrMatrix = fattenList 8 $ P.map (fattenList 8) smallMatrix
    writePng filepath (encodePng qrMatrix)

-- | given a filepath, read the QR code as a string in all lowercase
readQRString :: FilePath -> IO String
readQRString filepath = (map toLower) . init . (drop 8 . view _2) <$> readCreateProcessWithExitCode (shell $ "zbarimg " ++ filepath) ""

--readQRStrSec :: FilePath -> IO (Eiher JwtError BS.ByteString)
readQRStrSec filepath = do
    enc <- (map toLower) . init . (drop 8) . (view _2) <$> readCreateProcessWithExitCode (shell $ "zbarimg " ++ filepath) ""
    print $ resolveUpper $ BS.pack enc
    (fmap liftIO) . checkSig . resolveUpper $ (BS.pack) enc

-- | function applied to byteStrings before saving to QR code so that uppercase/lowercase signatures can be preserverd
preserveUpper :: BS.ByteString -> BS.ByteString
preserveUpper = lift pU
    where pU = concatMap (\c -> if c `elem` ['A'..'Z'] then ((toLower c) : "---") else return c)

-- | resolve coded string to string with uppercase
resolveUpper :: BS.ByteString -> BS.ByteString
resolveUpper = lift rU
    where rU = foldr (.) id (map (\s -> replace (s : "---") ((pure . toUpper) s)) ['a'..'z'])

lift :: (String -> String) -> (BS.ByteString -> BS.ByteString)
lift f = BS.pack . f . BS.unpack

--liftIO :: Either String BS.ByteString -> String
liftIO = either (show) (((++) "success! ") . show)

encodePng :: [[Word8]] -> T.Image Word8
encodePng matrix = Image dim dim vector
    where dim    = P.length matrix
          vector = V.map ((*255) . swapWord) $ V.fromList $ P.concat matrix

fattenList :: Int -> [a] -> [a]
fattenList i l = P.concat $ P.foldr ((:) . (P.replicate i)) [] l

swapWord :: Word8 -> Word8
swapWord 1 = 0
swapWord 0 = 1
