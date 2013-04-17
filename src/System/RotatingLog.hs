{-# LANGUAGE RecordWildCards #-}

module System.RotatingLog
  ( RotatingLog
  , mkRotatingLog
  , rotatedWrite
  ) where

import           Control.Concurrent.MVar
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Time
import           Data.Word
import           System.IO
import           System.Locale

------------------------------------------------------------------------------
-- | A size-limited rotating log.  Log filenames are of the format
-- prefix_timestamp.log.
data RotatingLog = RotatingLog
    { logInfo    :: MVar LogInfo
    , namePrefix :: String
    , sizeLimit  :: Word64
    }

data LogInfo = LogInfo
    { curHandle    :: Handle
    , bytesWritten :: !Word64
    }

logFileName :: String -> UTCTime -> FilePath
logFileName pre t = concat
    [pre, "_", formatTime defaultTimeLocale "%F_%T%Q" t, ".log"]

------------------------------------------------------------------------------
-- | Creates a rotating log given a prefix and size limit in bytes.
mkRotatingLog :: String -> Word64 -> IO RotatingLog
mkRotatingLog pre limit = do
    t <- getCurrentTime
    h <- openFile (logFileName pre t) AppendMode
    mvar <- newMVar $ LogInfo h 0
    return $ RotatingLog mvar pre limit

------------------------------------------------------------------------------
-- | Like "rotatedWrite'", but doesn't need a UTCTime.
rotatedWrite :: RotatingLog -> ByteString -> IO ()
rotatedWrite rlog bs = do
    t <- getCurrentTime
    rotatedWrite' rlog t bs

------------------------------------------------------------------------------
-- | Writes ByteString to a rotating log file.  If this write would exceed the
-- size limit, then the file is closed and a new file opened.  This function
-- takes a UTCTime to allow a cached time to be used to avoid a system call.
rotatedWrite' :: RotatingLog -> UTCTime -> ByteString -> IO ()
rotatedWrite' RotatingLog{..} t bs = do
    modifyMVar_ logInfo $ \LogInfo{..} -> do
        (h,b) <- if bytesWritten + len > sizeLimit
                   then do hClose curHandle
                           h <- openFile (logFileName namePrefix t) AppendMode
                           return (h,0)
                   else return (curHandle, bytesWritten)
        B.hPutStrLn h bs
        return $ LogInfo h (b+len)
  where
    len = fromIntegral $ B.length bs + 1

