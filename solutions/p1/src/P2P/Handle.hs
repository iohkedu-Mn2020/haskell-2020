{-|
Module      : P2P.Handle
Description : Concurrency-safe handles.
Copyright   : (c) Lars Brünjes, 2019
License     : MIT
Maintainer  : lars.bruenjes@iohk.io
Stability   : experimental
Portability : portable

This module contains a type of handles that serialize concurrent writers.
-}

module P2P.Handle
    ( Handle
    , Logger
    , createHandle
    , hClose
    , hPutStrLn
    , hPrint
    , hGetLine
    , createLogger
    , logString
    ) where

import           Control.Concurrent     (MVar, newMVar, withMVar)
import           Control.Monad.IO.Class (MonadIO (..))
import qualified System.IO              as IO

-- | A handle that serializes concurrent writers.
data Handle = Handle
    { hHandle :: IO.Handle
    , hLock   :: MVar ()
    }

instance Show Handle where
    show = show . hHandle

-- | Creates a 'Handle' from a 'IO.Handle'.
createHandle :: MonadIO m => IO.Handle -> m Handle
createHandle h = liftIO $ do
    v <- newMVar ()
    IO.hSetBuffering h IO.LineBuffering
    return Handle
        { hHandle = h
        , hLock   = v
        }

-- | Closes the 'Handle'.
hClose :: MonadIO m => Handle -> m ()
hClose h = liftIO $ IO.hClose $ hHandle h

-- | Writes the specified 'String' to the 'Handle',
-- serializing concurrent writers.
hPutStrLn :: MonadIO m => Handle -> String -> m ()
hPutStrLn h s = liftIO $ withMVar (hLock h) $ \() -> IO.hPutStrLn (hHandle h) s

-- | Prints the 'String'-representation of the given value to the 'Handle',
-- serializing concurrent writers.
hPrint :: (Show a, MonadIO m) => Handle -> a -> m ()
hPrint h = hPutStrLn h . show

-- | Reads a line from the 'Handle'. There must be no concurrent readers.
hGetLine :: MonadIO m => Handle -> m String
hGetLine = liftIO . IO.hGetLine . hHandle

-- | A logger (simply a wrapper aroung a 'Handle' for 'IO.stdout').
newtype Logger = Logger Handle

-- | Creates a 'Logger'. There should only be one such 'Logger' in the whole application.
createLogger :: MonadIO m => m Logger
createLogger = Logger <$> createHandle IO.stdout

-- | Logs the specified 'String' to the 'Logger'.
logString :: MonadIO m => Logger -> String -> m ()
logString (Logger h) = hPutStrLn h
