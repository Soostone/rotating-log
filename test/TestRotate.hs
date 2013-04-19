{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent
import qualified Data.ByteString.Char8 as B
import           Data.Time
import           System.RotatingLog

thread :: RotatingLog -> B.ByteString -> Int -> IO ()
thread log pre n = do
    rotatedWrite log (B.concat [pre, ": ", B.pack $ show n])
    thread log pre (n+1)

main = do
    log <- mkRotatingLog "foo" 1000000 (const $ return ())
    a <- forkIO $ thread log "a" 0
    b <- forkIO $ thread log "b" 0
    c <- forkIO $ thread log "c" 0
    d <- forkIO $ thread log "d" 0
    threadDelay 5000000
    killThread a
    killThread b
    killThread c
    killThread d
    putStrLn "done"
