{-|
Module      : P2P.Limit
Description : Limiting the number of connected peers.
Copyright   : (c) Lars Brünjes, 2019
License     : MIT
Maintainer  : lars.bruenjes@iohk.io
Stability   : experimental
Portability : portable

This module defines the agent whose task it is to limit the number of connected peers to the desired number.
-}

module P2P.Limit
    ( limitM
    ) where

import           Control.Monad          (forever)
import           Control.Monad.IO.Class (liftIO)
import           System.Random          (randomRIO)

import           P2P.Core

-- | Agent limiting the number of connected peers to the specified number.
limitM :: Int  -- ^ Maximal number of connected peers.
       -> M ()
limitM limit
    | limit < 0 = return ()
    | otherwise = forever $ do
        ps <- waitUntilOverLimitM limit
        i  <- liftIO $ randomRIO (0, length ps - 1)
        let peer = ps !! i
        logM $ "too many connected peers. Disconnecting from " ++ show peer
        finallyM
            (sendPeerM peer Quit)
            (removePeerM peer)
