module Main where

import Network.Socket
import System.IO 
import System.Directory
import Control.Concurrent
import Data.List.Split
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BSU 
import Magic

main :: IO ()
main = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 8080 0)
  listen sock 2
  putStrLn "Listening on 8080"
  sockHandler sock

sockHandler :: Socket -> IO ()
sockHandler sock = do
  (conn, _) <- accept sock
  handle <- socketToHandle conn ReadWriteMode
  hSetBuffering handle NoBuffering
  forkIO $ handleConn handle
  sockHandler sock

handleConn :: Handle -> IO ()
handleConn handle = do
  request <- hGetLine handle
  let (_: uri: _) = splitOn " " (head (lines request))
  fileExists <- doesFileExist (tail uri)
  if fileExists
  then do 
    magic <- magicOpen [MagicMime]
    magicLoadDefault magic
    mime <- magicFile magic (tail uri)
    contents <- B.readFile (tail uri) 
    B.hPut handle $ BSU.fromString "HTTP/1.1 200 OK\r\n"
    B.hPut handle $ BSU.fromString ("Content-Type: " ++ mime ++ "\r\n")
    B.hPut handle $ BSU.fromString "\r\n"
    B.hPut handle contents 
  else do 
    B.hPut handle $ BSU.fromString "HTTP/1.1 404 Not Found\r\n"
    B.hPut handle $ BSU.fromString "Content-Type: text/html\r\n"
    B.hPut handle $ BSU.fromString "\r\n"
    B.hPut handle $ BSU.fromString "<h1>404 Not Found</h1>"

  hClose handle

