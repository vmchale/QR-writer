-- | A few functions to deal with the image itself
module Data.QRCodes.Image (-- * Functions to convert to JuicyPixels `Image`
                          bsToImg
                          , objToImg
                          -- * Functions to sign and convert to `Image`
                          , bsToImgSec
                          , objToImgSec
                          -- * Functions to sign with user-supplied key and yield an `Image`
                          , bsToImgSec'
                          , objToImgSec'
                          ) where

import           Codec.Picture.Types    as T
import           Crypto.PubKey.RSA
import           Data.Aeson
import qualified Data.ByteString        as BS
import           Data.ByteString.Lazy   (toStrict)
import           Data.QRCode
import           Data.QRCodes.Signature
import           Data.QRCodes.Utils
import qualified Data.Vector.Storable   as V
import           Data.Word              (Word8)
import           Prelude                as P

-- | Creates a signed QR code from a strict bytestring and path to keyfile/path where the keyfile should be generated, yielding a JuicyPixels `Image`.
-- Note that QR codes may only contain a small number of characters, so encrypting can sometimes make an object too big to encode.
--
-- > bsToImgSec (BS.pack "hello") ".key.hk"
bsToImgSec :: BS.ByteString -> FilePath -> IO (T.Image Word8)
bsToImgSec string keyfile = bsToImg =<< (fmap preserveUpper . flip mkSigFile keyfile) string

-- | Sign a byteString with a given key
--
-- > bsToImgSec' (BS.pack "str") (generate 256 0x10001)
bsToImgSec' :: BS.ByteString -> (PublicKey, PrivateKey) -> IO (T.Image Word8)
bsToImgSec' string key = bsToImg =<< (fmap preserveUpper . flip mkSig key) string

-- | Encode an object as a JuicyPixels `Image` with a key in a given file.
objToImgSec :: (ToJSON a) => a -> FilePath -> IO (T.Image Word8)
objToImgSec obj = bsToImgSec (toStrict $ encode obj)

-- | Encode an object as a JuicyPixels `Image` with a key.
objToImgSec' :: (ToJSON a) => a -> (PublicKey, PrivateKey) -> IO (T.Image Word8)
objToImgSec' obj = bsToImgSec' (toStrict $ encode obj)

-- | Create a JuicyPixels `Image` from a `ByteString`
bsToImg :: BS.ByteString -> IO (T.Image Word8)
bsToImg input = do
    smallMatrix <- toMatrix <$> encodeByteString input Nothing QR_ECLEVEL_H QR_MODE_EIGHT False
    let qrMatrix = fattenList 8 $ map (fattenList 8) smallMatrix --consider using repa here, with the `onImg` thing with JuicyPixels
    pure $ encodePng qrMatrix

-- | Encode an object as a JuicyPixels `Image`
objToImg :: (ToJSON a) => a -> IO (T.Image Word8)
objToImg obj = let input = toStrict $ encode obj in bsToImg input

-- | Encode a JuicyPixels `Image` given a matrix
encodePng :: [[Word8]] -> T.Image Word8
encodePng matrix = Image dim dim vector
    where dim    = P.length matrix
          vector = V.map ((*255) . swapWord) $ V.fromList $ P.concat matrix

-- | To help scale the image up, e.g.
--
-- > fattenList 8 $ (map fattenList 8) smallMatrix
--
-- to scale @smallMatrix :: [[Word8]]@ by a factor of 8
fattenList :: Int -> [a] -> [a]
fattenList i l = P.concat $ P.foldr ((:) . P.replicate i) [] l
