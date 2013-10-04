{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.RotatingLog
-- Copyright   :  Soostone Inc
-- License     :  BSD3
--
-- Maintainer  :  admin@soostone.com
-- Stability   :  experimental
--
-- Convenient logging to a disk-based log file with automatic file
-- rotation based on size.
----------------------------------------------------------------------------

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
    }


data LogInfo = LogInfo
    { curHandle    :: Handle
    , bytesWritten :: !Word64
    }


curLogFileName :: String -> FilePath
curLogFileName = (++".log")


logFileName :: String -> UTCTime -> FilePath
logFileName pre t = concat
    [pre, "_", formatTime defaultTimeLocale "%Y_%m_%d_%H_%M_%S%Q" t, ".log"]


------------------------------------------------------------------------------
-- | Creates a rotating log given a prefix and size limit in bytes.
mkRotatingLog
    :: String
    -- ^ A prefix for the written log files.
    -> Word64
    -- ^ A size limit in bytes.
    -> (FilePath -> IO ())
    -- ^ An action to be performed on the finished file following
    -- rotation. For example, you could give a callback that moves or
    -- ships the files somewhere else.
    -> IO RotatingLog
mkRotatingLog pre limit pa = do
    let fp = curLogFileName pre
    h <- openFile fp AppendMode
    len <- hFileSize h
    mvar <- newMVar $ LogInfo h (fromIntegral len)
    return $ RotatingLog mvar pre limit pa


------------------------------------------------------------------------------
-- | Like "rotatedWrite'", but doesn't need a UTCTime and obtains it
-- with a syscall.
rotatedWrite :: RotatingLog -> ByteString -> IO ()
rotatedWrite rlog bs = do
    t <- getCurrentTime
    rotatedWrite' rlog t bs


------------------------------------------------------------------------------
-- | Writes ByteString to a rotating log file.  If this write would exceed the
-- size limit, then the file is closed and a new file opened.  This function
-- takes a UTCTime to allow a cached time to be used to avoid a system call.
--
-- Please note this function does NOT implicitly insert a newline at
-- the end of the string you provide. This is so that it can be used
-- to log non-textual streams such as binary serialized or compressed
-- content.
rotatedWrite' :: RotatingLog -> UTCTime -> ByteString -> IO ()
rotatedWrite' RotatingLog{..} t bs = do
    modifyMVar_ logInfo $ \LogInfo{..} -> do
        (h,b) <- if bytesWritten + len > sizeLimit
                   then do hClose curHandle
                           let newFile = logFileName namePrefix t
                           renameFile curFile newFile
                           postAction newFile
                           h <- openFile curFile AppendMode
                           return (h, 0)
                   else return (curHandle, bytesWritten)
        B.hPutStr h bs
        return $! LogInfo h (len + b)
  where
    len = fromIntegral $ B.length bs
    curFile = curLogFileName namePrefix


-------------------------------------------------------------------------------
-- | A built-in post-rotate action that moves the finished file to a
-- given archive location.
archiveFile
    :: FilePath
    -- ^ A target archive directory
    -> (FilePath -> IO ())
archiveFile archive fp =
    let (_, fn) = splitFileName fp
        target = archive </> fn
    in do
        createDirectoryIfMissing True archive
        renameFile fp target



