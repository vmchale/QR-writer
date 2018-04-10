-- | Miscellaneous helper functions that don't fit anywhere else
module Data.QRCodes.Utils (replace, preserveUpper, resolveUpper, swapWord, liftEither) where

import qualified Data.ByteString.Char8 as BS
import           Data.Char             (toLower, toUpper)
import           Data.List             (intercalate)
import           Data.List.Split       (splitOn)
import           Data.Word             (Word8)

replace :: String -> String -> String -> String
replace ndl target haystack = intercalate target $ splitOn ndl haystack

-- | function applied to byteStrings before saving to QR code so that uppercase/lowercase signatures can be preserverd
preserveUpper :: BS.ByteString -> BS.ByteString
preserveUpper = lift pU
    where pU = concatMap (\c -> if c `elem` ['A'..'Z'] then toLower c : "!" else return c)

-- | resolve coded string to string with uppercase
resolveUpper :: BS.ByteString -> BS.ByteString
resolveUpper = lift rU
    where rU = foldr ((.) . (\s -> replace (s : "!") ((pure . toUpper) s))) id ['a'..'z']

-- | given a function on strings, make it act on byteStrings
lift :: (String -> String) -> (BS.ByteString -> BS.ByteString)
lift f = BS.pack . f . BS.unpack

-- | helper function to lift Either values to IO in our case
liftEither :: (Show b, Monad m) => (t -> m a) -> Either b t -> m a
liftEither = either (error . show)

swapWord :: Word8 -> Word8
swapWord 1 = 0
swapWord 0 = 1
