{-|
Module      : P2P.Discovery
Description : Peer discovery.
Copyright   : (c) Lars Brünjes, 2019
License     : MIT
Maintainer  : lars.bruenjes@iohk.io
Stability   : experimental
Portability : portable

This module contains and agent who permanently tries to discover new peers.
-}

module P2P.Discovery
    ( discoveryM
    ) where

import           Control.Concurrent     (threadDelay)
import           Control.Monad          (forever, when)
import           Control.Monad.IO.Class (liftIO)
import           Network                (PortID (PortNumber), connectTo)
import           System.Random          (randomRIO)

import           P2P.Core
import           P2P.Handle
import           P2P.PeerHandling

-- | Tries to permanently discover new peers.
-- The agent picks a random (connected or disconnected) peer, then contacts him
-- and asks him for his list of peers.
discoveryM :: Microseconds -- ^ Time between new attempts to discover new peers.
           -> M ()
discoveryM delay = forever $ do
    liftIO $ threadDelay delay
    ps <- getPeersM
    let l = length ps
    when (l > 0) $ do
        i  <- liftIO $ randomRIO (0, l - 1)
        let p = ps !! i
        logM $ "trying to ask peer " ++ show p ++ " for its peers"
        b <- connectToPeer p
        when b $ catchM (sendPeerM p GetPeers) $ const $ return ()

connectToPeer :: Peer -> M Bool
connectToPeer p@(Peer host port) = do
    mh <- getPeerHandleM p
    case mh of
        Just _  -> return True
        Nothing -> flip catchM (const $ return False) $ do
            h' <- liftIO $ connectTo host $ PortNumber port
            h  <- createHandle h'
            b <- addPeerM p h
            if b then do
                    _ <- forkFinallyM
                        (handlePeer p)
                        (const $ hClose h)
                    (Peer myHost myPort) <- selfM
                    sendPeerM p $ Connect myHost myPort
                    return True
                 else return False
