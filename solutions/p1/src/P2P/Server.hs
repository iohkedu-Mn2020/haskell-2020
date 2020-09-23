{-|
Module      : P2P.Server
Description : Peer-to-peer server.
Copyright   : (c) Lars Brünjes, 2019
License     : MIT
Maintainer  : lars.bruenjes@iohk.io
Stability   : experimental
Portability : portable

This module defines the peer-to-peer server.
-}

module P2P.Server
    ( PortNumber
    , serverM
    ) where

import Control.Monad.Reader
import Network              (PortID (..), PortNumber, accept, listenOn)

import P2P.Core
import P2P.Handle
import P2P.PeerHandling

-- | Runs the peer-to-peer server.
serverM :: M ()
serverM = do
    self <- selfM
    s    <- let Peer _ port = self in liftIO $ listenOn $ PortNumber port
    logM $ "server " ++ show self ++ " started"
    forever $ do
        (h, host, port') <- liftIO $ accept s
        logM $ "accepted connection from " ++ host ++ ", port " ++ show port'
        h' <- createHandle h
        forkFinallyM
            (handleClient h')
            (const $ do
                logM $ "connection to " ++ host ++ ", port " ++ show port' ++ " closed"
                hClose h')

handleClient :: Handle -> M ()
handleClient h = do
    msg <- receiveHandleM h
    case msg of
        Connect host port -> do
            let peer = Peer host port
            b <- addPeerM peer h
            if b then do
                    logM $ "not already connected to " ++ show peer
                    handlePeer peer
                 else do
                    logM $ "already connected to " ++ show peer ++ ", closing connection"
                    sendPeerM peer Quit
        _                 -> do
            logM "didn't receive Connect message - terminating connection"
            sendHandleM h Quit
