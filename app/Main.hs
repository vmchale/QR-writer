import QRCodes
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import System.Environment (getArgs)

main :: IO ()
main = do
    pipeIn <- B.getContents
    filepath <- fmap (flip (!!) 0) getArgs
    byteStringToQR pipeIn filepath
    (mkSig (pack "vanessa") >>= checkSig) >>= print
