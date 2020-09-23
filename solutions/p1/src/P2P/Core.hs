{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Module      : P2P.Core
Description : Core types and functions.
Copyright   : (c) Lars Brünjes, 2019
License     : MIT
Maintainer  : lars.bruenjes@iohk.io
Stability   : experimental
Portability : portable

This module contains the core data types and functions to operate on them.
-}

module P2P.Core
    ( Message (..)
    , Peer (..)
    , Tx
    , Microseconds
    , M
    , runIO
    , forkFinallyM
    , finallyM
    , catchM
    , raceM
    , logM
    , stateM
    , sendHandleM
    , sendPeerM
    , sendAllPeersM
    , receiveHandleM
    , receivePeerM
    , selfM
    , addPeerM
    , removePeerM
    , getPeersM
    , waitUntilOverLimitM
    , getPeerHandleM
    , addPeersM
    , newTxM
    , createTxM
    ) where

import           Control.Concurrent          (ThreadId, forkFinally)
import           Control.Concurrent.Async    (race_)
import           Control.Concurrent.STM      (atomically, retry)
import           Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar,
                                              readTVarIO, writeTVar)
import           Control.Exception           (SomeException, catch, finally)
import           Control.Monad               (forM_, when)
import           Control.Monad.Reader        (MonadIO (..), MonadReader (..),
                                              ReaderT (..), asks)
import           Data.List                   (foldl')
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as M
import           Data.Maybe                  (fromMaybe, isJust, isNothing)
import           Network                     (HostName, PortNumber)
import           Network.Info                (MAC (..), getNetworkInterfaces,
                                              ipv4, mac)
import           System.Random               (randomRIO)
import           Text.Read                   (readMaybe)

import           P2P.Handle

-- | A peer, given by his host name and port number.
data Peer = Peer HostName PortNumber
    deriving (Show, Read, Eq, Ord)

-- | A (dummy) transaction.
type Tx = Int

-- | The type of messages that peers use to communicate with eachother.
data Message =
      Connect HostName PortNumber -- ^ Sent by the peer making the connection to announce his own host name and port number.
    | GetPeers                    -- ^ Asking a peer for his list of known peers.
    | Status [Peer]               -- ^ Answer to a 'GetPeers' message.
    | Newtx Tx                    -- ^ Announcing a new transaction.
    | Oldtx Tx Tx                 -- ^ Announcing that a transaction was not the most recent one and giving the actual most recent transaction.
    | Quit                        -- ^ Terminating the connection.
    | Unknown String              -- ^ Declaring that a received message was illegal.
    deriving (Show, Read, Eq, Ord)

data ServerState = ServerState
    { ssTx     :: TVar Tx
    , ssPeers  :: TVar (Map Peer (Maybe Handle))
    , ssLogger :: Logger
    , ssSelf   :: Peer
    }

-- | Time in microseconds.
type Microseconds = Int

initServerState :: PortNumber -> [Peer] -> IO ServerState
initServerState port peers = do
    logger    <- createLogger
    vtx       <- newTVarIO 0
    vps       <- newTVarIO $ M.fromList [(p, Nothing) | p <- peers]
    Just host <- getOwnHost
    return ServerState
        { ssTx     = vtx
        , ssPeers  = vps
        , ssLogger = logger
        , ssSelf   = Peer host port
        }

-- | Custom monad to use; it has access to the server state.
newtype M a = M (ReaderT ServerState IO a)
    deriving (Functor, Applicative, Monad, MonadIO)

-- | Runs an 'M'-computation in 'IO'.
runIO :: PortNumber -- ^ Port number of the server.
      -> [Peer]     -- ^ Initially known peers.
      -> M a        -- ^ The computation to run.
      -> IO a
runIO port peers (M m) = do
    ss <- initServerState port peers
    runReaderT m ss

-- | Version of 'IO.finally' adapted to work with 'M'.
finallyM :: M a -> M b -> M a
finallyM (M ma) (M mb) = M $ ReaderT $ \ss -> finally
    (runReaderT ma ss)
    (runReaderT mb ss)

-- | Version of 'IO.forkFinally' adapted to work with 'M'.
forkFinallyM :: M a -> (Either SomeException a -> M ()) -> M ThreadId
forkFinallyM (M m) h = M $ ReaderT $ \ss -> forkFinally
    (runReaderT m ss)
    (\e -> let M n = h e in runReaderT n ss)

-- | Version of 'IO.catch' adapted to work with 'M'.
catchM :: M a -> (SomeException -> M a) -> M a
catchM (M m) h = M $ ReaderT $ \ss -> catch
    (runReaderT m ss)
    (\e -> let M n = h e in runReaderT n ss)

-- | Version of 'Control.Concurrent.Async.race' adapted to work with 'M'.
raceM :: M a -> M b -> M ()
raceM (M m) (M n) = M $ ReaderT $ \ss -> race_
    (runReaderT m ss)
    (runReaderT n ss)

-- | Logs a string to the console.
logM :: String -> M ()
logM s = M $ do
    ss <- ask
    logString (ssLogger ss) s

-- | Gets the status, i.e. own host name and port number, list of peers
-- (with indicator whether those peers are connected or not) and latest
-- transaction.
stateM :: M (Peer, [(Peer, Bool)], Tx)
stateM = do
    ss <- M ask
    ps <- liftIO $ readTVarIO $ ssPeers ss
    tx <- liftIO $ readTVarIO $ ssTx    ss
    let self = ssSelf ss
        ps'  = [(p, isJust mh) | (p, mh) <- M.toList ps]
    return (self, ps', tx)

handlePrefix :: Handle -> String
handlePrefix h = "handle " ++ show h ++ ": "

peerPrefix :: Peer -> String
peerPrefix p = "peer " ++ show p ++ ": "

handleForPeer :: Peer -> M Handle
handleForPeer p = do
    ss <- M ask
    ps <- liftIO $ readTVarIO $ ssPeers ss
    case M.lookup p ps of
        Just (Just h) -> return h
        _             -> liftIO $ ioError $ userError $ "connection to peer " ++ show p ++ " lost"

sendM :: Handle -> String -> Message -> M ()
sendM h prefix msg = do
    logM $ prefix ++ "sending " ++ show msg
    hPrint h msg

-- | Sends a 'Message' to the 'Handle'.
sendHandleM :: Handle -> Message -> M ()
sendHandleM h = sendM h $ handlePrefix h

-- | Sends a 'Message' to the 'Peer'.
sendPeerM :: Peer -> Message -> M ()
sendPeerM p msg = do
    h <- handleForPeer p
    sendM h (peerPrefix p) msg

-- | Sends a 'Message' to all connected peers.
sendAllPeersM :: Message -> M ()
sendAllPeersM msg = do
    v  <- M $ asks ssPeers
    ps <- liftIO $ readTVarIO v
    let ps' = [p | (p, mh) <- M.toList ps, isJust mh]
    forM_ ps' $ \p -> sendPeerM p msg `catchM` (const $ removePeerM p)

receiveM :: Handle -> String -> M Message
receiveM h prefix = do
    s <- hGetLine h
    case readMaybe s of
        Nothing  -> do
            logM $ prefix ++ "received unknown message '" ++ s ++ "'"
            sendM h prefix $ Unknown s
            liftIO $ ioError $ userError $ "unknown message '" ++ s ++ "'"
        Just msg -> do
            logM $ prefix ++ "received message " ++ show msg
            return msg

-- | Receives a 'Message' from the 'Handle'.
receiveHandleM :: Handle -> M Message
receiveHandleM h = receiveM h $ handlePrefix h

-- | Receives a 'Message' from the connected 'Peer'.
receivePeerM :: Peer -> M Message
receivePeerM p = do
    h <- handleForPeer p
    receiveM h $ peerPrefix p

-- | Gets the node's host name and port number.
selfM :: M Peer
selfM = ssSelf <$> M ask

getOwnHost :: IO (Maybe HostName)
getOwnHost = do
    xs <- map ipv4 . filter (\x -> mac x /= MAC 0 0 0 0 0 0) <$> getNetworkInterfaces
    return $ case xs of
        []      -> Nothing
        (x : _) -> Just $ show x

-- | Associates the 'Peer' with the 'Handle. The return value indicates whether
-- that 'Peer' had previously been disconnected.
addPeerM :: Peer -> Handle -> M Bool
addPeerM p h = do
    logM $ "connected to peer " ++ show p
    ss <- M ask
    liftIO $ atomically $ do
        ps <- readTVar $ ssPeers ss
        case M.lookup p ps of
            Just (Just _) -> return False
            _             -> do
                writeTVar (ssPeers ss) $ M.insert p (Just h) ps
                return True

-- | Disconnects the 'Peer' and closes his 'Handle'.
removePeerM :: Peer -> M ()
removePeerM p = do
    logM $ "removing handle for peer " ++ show p
    v <- M $ asks ssPeers
    mh <- liftIO $ atomically $ do
        ps <- readTVar v
        let mmh = M.lookup p ps
        case mmh of
            Just (Just h) -> do
                writeTVar v $ M.insert p Nothing ps
                return $ Just h
            _             -> return Nothing
    forM_ mh hClose

-- | Returns all peers (both connected and disconnected).
getPeersM :: M [Peer]
getPeersM = do
    ss <- M ask
    ps <- liftIO $ readTVarIO $ ssPeers ss
    return $ M.keys ps

-- | Waits until the number of connected peers is greater than the specified
-- limit, then returns the connected peers.
waitUntilOverLimitM :: Int      -- ^ Limit for the number of connected peers.
                    -> M [Peer]
waitUntilOverLimitM limit = do
    v <- M $ asks ssPeers
    liftIO $ atomically $ do
        ps <- readTVar v
        let ps' = [p | (p, mh) <- M.toList ps, isJust mh]
        when (length ps' <= limit) retry
        return ps'

-- | Gets a 'Handle' for the 'Peer'. Will return 'Nothing' if the 'Peer' is
-- disconnected.
getPeerHandleM :: Peer -> M (Maybe Handle)
getPeerHandleM p = do
    v  <- M $ asks ssPeers
    ps <- liftIO $ readTVarIO v
    return $ fromMaybe Nothing $ M.lookup p ps

-- | Adds the peers to the list of known peers (without connecting to them).
addPeersM :: [Peer] -> M ()
addPeersM ps = do
    v    <- M $ asks ssPeers
    self <- M $ asks ssSelf
    liftIO $ atomically $ do
        m <- readTVar v
        writeTVar v $ foldl' f m $ filter (/= self) ps
  where
    f :: Map Peer (Maybe Handle) -> Peer -> Map Peer (Maybe Handle)
    f m p = M.alter g p m

    g :: Maybe (Maybe Handle) -> Maybe (Maybe Handle)
    g Nothing   = Just Nothing
    g (Just mh) = Just mh

-- | Tries to add a new transaction.
newTxM :: Tx           -- ^ The transaction to add.
       -> M (Maybe Tx) -- ^ Returns 'Nothing' if the transaction has been added and @'Just' tx@ if there is already a newer transaction @tx@.
newTxM tx = do
    v   <- M $ asks ssTx
    mtx <- liftIO $ atomically $ do
        tx' <- readTVar v
        if tx' < tx
            then writeTVar v tx >> return Nothing
            else return $ Just tx'
    when (isNothing mtx) $ logM $ "new transaction " ++ show tx
    return mtx

-- | Creates a new random transaction and sets it as newest known transaction.
createTxM :: M Tx
createTxM = do
    v <- M $ asks ssTx
    d <- liftIO $ randomRIO (1, 10)
    liftIO $ atomically $ do
        tx' <- readTVar v
        let tx = tx' + d
        writeTVar v tx
        return tx
