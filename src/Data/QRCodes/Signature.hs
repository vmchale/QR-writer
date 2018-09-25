-- | Functions associated with signing the JSON records
module Data.QRCodes.Signature where

import           Control.Monad
import           Crypto.PubKey.RSA
import qualified Data.ByteString.Char8 as BS
import           Data.QRCodes.Utils
import           Jose.Jwa              (JwsAlg (RS256))
import           Jose.Jws
import           Jose.Jwt              (JwtError, unJwt)
import           System.Directory

-- | Verify a signed token with a key
checkSigFile :: BS.ByteString -> FilePath -> IO (Either JwtError BS.ByteString)
checkSigFile tok filepath = do
    key <- read <$> readFile filepath :: IO (PublicKey, PrivateKey)
    checkSig tok key

-- | Verify a signed token with a key from a given filepath
checkSig :: BS.ByteString -> (PublicKey, PrivateKey) -> IO (Either JwtError BS.ByteString)
checkSig tok key = do
    let jws = rsaDecode (fst key) tok
    pure $ fmap snd jws

-- | Sign a token.
-- If the key file does not exist, a new key will be generated.
mkSigFile :: BS.ByteString -> FilePath -> IO BS.ByteString
mkSigFile string filepath = do
    switch <- doesFileExist filepath
    unless switch $ do
        putStrLn "generating key..."
        key <- generate 256 0x10001
        writeFile filepath (show key)
    key' <- read <$> readFile filepath :: IO (PublicKey, PrivateKey)
    mkSig string key'

-- | Sign a token with a key.
-- (note that we must pass in a processed token since there is no uppercase/lowercase for QR codes)
--
-- > mkSig (BS.pack "hello") (generate 256 0x10001)
mkSig :: BS.ByteString -> (PublicKey, PrivateKey) -> IO BS.ByteString
mkSig string key = do
    signedToken <- rsaEncode RS256 (snd key) string
    let signed = fmap unJwt signedToken
    liftEither id (return <$> signed)

