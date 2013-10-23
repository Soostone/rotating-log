{-# LANGUAGE OverloadedStrings #-}

module Main where

-------------------------------------------------------------------------------
import           Control.Concurrent
import qualified Data.ByteString.Char8 as B
import           System.IO
import           System.RotatingLog
-------------------------------------------------------------------------------


thread :: RotatingLog -> B.ByteString -> Int -> IO ()
thread _log pre n = do
    rotatedWrite _log (B.concat [pre, ": ", B.pack $ show n, "\n"])
    thread _log pre (n+1)

main :: IO ()
main = do
    _log <- mkRotatingLog "foo" 1000000 LineBuffering (archiveFile "logs")
    a <- forkIO $ thread _log "a" 0
    b <- forkIO $ thread _log "b" 0
    c <- forkIO $ thread _log "c" 0
    d <- forkIO $ thread _log "d" 0
    threadDelay 5000000
    killThread a
    killThread b
    killThread c
    killThread d
    putStrLn "done"
