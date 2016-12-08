{-# LANGUAGE GADTs            #-}
{-# LANGUAGE FlexibleContexts #-}

module QRCodes where--(createSecureQRCode
              --, createQRCode
              --, byteStringToQR
              --) where

import Data.Aeson
import Data.QRCode
import Codec.Picture.Types as T
import Codec.Picture.Png (writePng)
import Data.Word (Word8)
import qualified Data.Vector.Storable as V
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString as BS
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
import Jose.Jwa (JwsAlg (RS512))
import Data.Either (either)
import Data.Bits ((.&.))
import Control.Applicative ((<$>))

checkSig :: BS.ByteString -> IO (Either JwtError BS.ByteString)
checkSig tok = do
    key <- read <$> readFile "key.hk"
    let jws = rsaDecode key tok
    return $ fmap (view _2) jws

mkSig :: BS.ByteString -> IO BS.ByteString
mkSig string = do
    switch <- doesFileExist "~/.key.hk"
    if not switch then do
        putStrLn "generating key..."
        key <- Cr.generate 512 0x10001
        writeFile ".key.hk" (show key)
    else
        return ()
    key' <- read <$> readFile "~/.key.hk" :: IO (Cr.PublicKey, Cr.PrivateKey)
    signedToken <- rsaEncode RS512 (view _2 key') string
    let signed = fmap unJwt signedToken
    liftEither id (return <$> signed)

byteStringToQRSec :: BS.ByteString -> FilePath -> IO ()
byteStringToQRSec string filepath = (flip byteStringToQR filepath) =<< (mkSig string)

createSecureQRCode :: (ToJSON a) => a -> FilePath -> IO ()
createSecureQRCode object filepath = byteStringToQRSec (toStrict $ encode object) filepath

liftEither :: (Show b, Monad m) => (t -> m a) -> Either b t -> m a
liftEither = either (fail . show)

createQRCode :: (ToJSON a) => a -> FilePath -> IO ()
createQRCode object filepath = let input = toStrict $ encode object in byteStringToQR input filepath

byteStringToQR :: BS.ByteString -> FilePath -> IO ()
byteStringToQR input filepath = do
    smallMatrix <- toMatrix <$> encodeByteString input Nothing QR_ECLEVEL_H QR_MODE_EIGHT False
    let qrMatrix = fattenList 8 $ P.map (fattenList 8) smallMatrix
    writePng filepath (encodePng qrMatrix)

encodePng :: [[Word8]] -> T.Image Word8
encodePng matrix = Image dim dim vector
    where dim    = P.length matrix
          vector = V.map ((*255) . swapWord) $ V.fromList $ P.concat matrix

fattenList :: Int -> [a] -> [a]
fattenList i l = P.concat $ P.foldr ((:) . (P.replicate i)) [] l

swapWord :: Word8 -> Word8
swapWord 1 = 0
swapWord 0 = 1
