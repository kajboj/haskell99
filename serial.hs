import System.IO
import System.Posix.Terminal
import System.Serial
import Control.Concurrent

main :: IO ()
main = do
  handle <- openSerial "/dev/ttyUSB0" B9600 8 One Even Software
  putStrLn "waiting"
  threadDelay 2000000
  putStrLn "done waiting"

  readLoop handle 0

  hClose handle

chars = "abcdefgh"

readLoop :: Handle -> Int -> IO ()
readLoop handle i = do 
  hPutChar handle (chars !! (i `mod` length chars)) 
  hFlush handle
  c <- hGetLine handle
  putStrLn c
  --c <- hGetChar handle
  --putStrLn [c]
  readLoop handle (i+1)