-- | Functions associated with signing the JSON records
module Data.QRCodes.Signature where

import Jose.Jws
import qualified Data.ByteString.Char8 as BS
import Jose.Jwt (unJwt, JwtError)
import Crypto.PubKey.RSA
import Control.Lens
import System.Directory
import Jose.Jwa (JwsAlg (RS256))
import Data.QRCodes.Utils

-- | Verify a signed token with a key
checkSigFile :: BS.ByteString -> FilePath -> IO (Either JwtError BS.ByteString)
checkSigFile tok filepath = do
    key <- read <$> readFile filepath :: IO (PublicKey, PrivateKey)
    checkSig tok key

-- | Verify a signed token with a key from a given filepath
checkSig :: BS.ByteString -> (PublicKey, PrivateKey) -> IO (Either JwtError BS.ByteString)
checkSig tok key = do
    let jws = rsaDecode (view _1 key) tok
    return $ fmap (view _2) jws

-- | Sign a token. 
-- If the key file does not exist, a new key will be generated.
mkSigFile :: BS.ByteString -> FilePath -> IO BS.ByteString
mkSigFile string filepath = do
    switch <- doesFileExist filepath
    if not switch then do
        putStrLn "generating key..."
        key <- generate 256 0x10001
        writeFile filepath (show key)
    else
        return ()
    key' <- read <$> readFile filepath :: IO (PublicKey, PrivateKey)
    mkSig string key'

-- | Sign a token with a key.
-- (note that we must pass in a processed token since there is no uppercase/lowercase for QR codes)
--
-- > mkSig (BS.pack "hello") (generate 256 0x10001)
mkSig :: BS.ByteString -> (PublicKey, PrivateKey) -> IO BS.ByteString
mkSig string key = do
    signedToken <- rsaEncode RS256 (view _2 key) string
    let signed = fmap unJwt signedToken
    liftEither id (return <$> signed)
  
