import System.Process

main :: IO ()
main = readCreateProcess (shell "echo 'hello friend' | qrpipe write -v output.png") "" >>= putStrLn
