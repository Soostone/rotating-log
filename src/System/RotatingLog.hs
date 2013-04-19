{-# LANGUAGE RecordWildCards #-}

module System.RotatingLog
  (

  -- * Core API

    RotatingLog
  , mkRotatingLog
  , rotatedWrite
  , rotatedWrite'

  -- * Built-In Post-Rotate Actions
  , archiveFile

  ) where

-------------------------------------------------------------------------------
import           Control.Concurrent.MVar
import           Data.ByteString.Char8   (ByteString)
import qualified Data.ByteString.Char8   as B
import           Data.Time
import           Data.Word
import           System.Directory
import           System.FilePath.Posix
import           System.IO
import           System.Locale
-------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | A size-limited rotating log.  Log filenames are of the format
-- prefix_timestamp.log.
data RotatingLog = RotatingLog
    { logInfo    :: MVar LogInfo
    , namePrefix :: String
    , sizeLimit  :: Word64
    , postAction :: FilePath -> IO ()
    -- ^ An action to be performed on the finished file after
    -- rotation.
    }

data LogInfo = LogInfo
    { curHandle    :: Handle
    , curFile      :: FilePath
    , bytesWritten :: !Word64
    }

logFileName :: String -> UTCTime -> FilePath
logFileName pre t = concat
    [pre, "_", formatTime defaultTimeLocale "%F_%T%Q" t, ".log"]

------------------------------------------------------------------------------
-- | Creates a rotating log given a prefix and size limit in bytes.
mkRotatingLog
    :: String
    -- ^ A prefix for the written log files
    -> Word64
    -- ^ A size limit in bytes
    -> (FilePath -> IO ())
    -- ^ A post-rotate action to be run on the finalized file.
    -> IO RotatingLog
mkRotatingLog pre limit pa = do
    t <- getCurrentTime
    let fp = logFileName pre t
    h <- openFile fp AppendMode
    mvar <- newMVar $ LogInfo h fp 0
    return $ RotatingLog mvar pre limit pa

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
    modifyMVar_ logInfo $ \li@LogInfo{..} -> do
        (h,fp,b) <- if bytesWritten + len > sizeLimit
                   then do hClose curHandle
                           postAction curFile
                           let fp = logFileName namePrefix t
                           h <- openFile fp AppendMode
                           return (h,fp, 0)
                   else return (curHandle, curFile, bytesWritten)
        B.hPutStrLn h bs
        return $! LogInfo h fp (len + b)
  where
    len = fromIntegral $ B.length bs + 1




-------------------------------------------------------------------------------
-- | A built-in post-rotate action that moves the finished file to an
-- archive location.
archiveFile
    :: FilePath
    -- ^ A target archive directory
    -> (FilePath -> IO ())
archiveFile fp archive =
    let (dir, fn) = splitFileName fp
        target = archive </> fn
    in do
        createDirectoryIfMissing True archive
        renameFile fp target



