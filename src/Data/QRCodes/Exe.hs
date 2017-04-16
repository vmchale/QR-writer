-- | Parse options applicatively and read or write secure or non-secure QR codes.
module Data.QRCodes.Exe where

import Options.Applicative
import Data.Aeson
import qualified Data.ByteString as B
import System.Environment (getArgs) --fix soon!
import Data.QRCodes
import Data.Monoid

-- | Data type for the executable comprising the command, whether to sign, whether to verify it worked, and the output filename
data Prog = Prog { cmd     :: Com
                 , secured :: Bool
                 , verify  :: Bool
                 , file    :: String}

-- | Command is either read or write
data Com = Input | Output

-- | main exec function
exec :: IO ()
exec = execParser full >>= act
    where 
        full = info (helper <*> program)
            ( fullDesc
            <> progDesc "Read/Write QR Codes with files"
            <> header "qrpipe - QR utilities made in Haskell" )

-- | Takes a `Prog` and returns the appropriate IO action
act :: Prog -> IO ()
act (Prog Output True True filepath) = do
    pipeIn <- getContents
    createSecureQRCode pipeIn ".key.hk" filepath
    (readQRStrSec filepath ".key.hk" :: IO String) >>= print
act (Prog Output False True filepath) = do
    pipeIn <- B.getContents
    byteStringToQR pipeIn filepath
    readQRString filepath >>= print
act (Prog Output True False filepath) = do
    pipeIn <- getContents
    createSecureQRCode (pipeIn) ".key.hk" filepath
act (Prog Output False False filepath) = do
    pipeIn <- B.getContents
    byteStringToQR pipeIn filepath
act (Prog Input True _ filepath) = 
    (readQRStrSec filepath ".key.hk" :: IO String) >>= print
act (Prog Input False _ filepath) = 
    readQRString filepath >>= print

-- | Parser for the command line
program :: Parser Prog
program = Prog
    <$> subparser
        ( command "write" (info (pure Output)
            ( progDesc "Create a QR Code from stdin" ))
        <> command "read" (info (pure Input)
            ( progDesc "Read a QR code from file" )))
    <*> switch
        ( long "signed"
        <> short 's'
        <> help "Whether to sign the QR code" )
    <*> switch
        ( long "verify"
        <> short 'v'
        <> help "Attempt to read the resultant file?" )
    <*> argument str (metavar "FILE")
