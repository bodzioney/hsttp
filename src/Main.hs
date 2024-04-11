module Main where

import Control.Concurrent
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BSU
import Data.List.Split
import Magic
import Network.Socket
import System.Directory
import System.IO

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

headers :: BSU.ByteString
headers =
  BSU.fromString
    "Content-Security-Policy: frame-ancestors 'none'\r\n\
    \X-Frame-Options: DENY\r\n\
    \X-Content-Type-Options: nosniff\r\n\
    \Referrer-Policy: same-origin\r\n"

handleConn :: Handle -> IO ()
handleConn handle = do
  request <- hGetLine handle
  let (_ : uri : _) = splitOn " " (head (lines request))
  fileExists <- doesFileExist (tail uri)
  if fileExists
    then do
      magic <- magicOpen [MagicMime]
      magicLoadDefault magic
      mime <- magicFile magic (tail uri)
      contents <- B.readFile (tail uri)
      B.hPut handle $ BSU.fromString "HTTP/1.1 200 OK\r\n"
      B.hPut handle headers
      B.hPut handle $ BSU.fromString ("Content-Type: " ++ mime ++ "\r\n")
      B.hPut handle $ BSU.fromString "\r\n"
      B.hPut handle contents
    else do
      B.hPut handle $ BSU.fromString "HTTP/1.1 404 Not Found\r\n"
      B.hPut handle $ BSU.fromString "Content-Type: text/html\r\n"
      B.hPut handle headers
      B.hPut handle $ BSU.fromString "\r\n"
      B.hPut handle $ BSU.fromString "<h1>404 Not Found</h1>"

  hClose handle
