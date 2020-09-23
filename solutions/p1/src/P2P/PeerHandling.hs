{-|
Module      : P2P.Core
Description : Handling a connected peer.
Copyright   : (c) Lars Brünjes, 2019
License     : MIT
Maintainer  : lars.bruenjes@iohk.io
Stability   : experimental
Portability : portable

This module contains the protocol for handling a connected peer.
-}

module P2P.PeerHandling
    ( handlePeer
    ) where

import           P2P.Core

-- | Handles a connected peer.
handlePeer :: Peer -- ^ The connected peer to handle.
           ->  M ()
handlePeer p = do
    logM $ "handling peer " ++ show p
    finallyM
        go
        (removePeerM p)
  where
    go :: M ()
    go = do
        msg <- receivePeerM p
        case msg of
            Quit          -> logM "peer closed connection"
            (Connect _ _) -> logM "peer sent second Connect message" >> sendPeerM p Quit
            GetPeers      -> do
                ps <- getPeersM
                sendPeerM p $ Status ps
                go
            (Unknown _)   -> go
            (Oldtx _ tx)  -> newTxM tx >> go
            (Newtx tx)    -> do
                mtx <- newTxM tx
                case mtx of
                    Nothing  -> return ()
                    Just tx' -> sendPeerM p $ Oldtx tx tx'
                go
            (Status ps)   -> addPeersM ps >> go
