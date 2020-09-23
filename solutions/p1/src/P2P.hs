{-|
Module      : P2P
Description : Top-level function to start the peer-to-peer protocol.
Copyright   : (c) Lars Brünjes, 2019
License     : MIT
Maintainer  : lars.bruenjes@iohk.io
Stability   : experimental
Portability : portable

This module contains the high level function needed to start the peer-to-peer protocol.
-}

module P2P
    ( Config
    , parseConfig
    , node
    ) where

import           P2P.Config
import           P2P.Core
import           P2P.Discovery
import           P2P.Limit
import           P2P.Server
import           P2P.Status
import           P2P.TxGenerator

-- | Starts a node with the specified configuration.
node :: Config -> IO ()
node cfg = runIO (cfgPort cfg) (cfgPeers cfg) $
            serverM
    `raceM` txGeneratorM (cfgTMin cfg) (cfgTMax cfg)
    `raceM` discoveryM (cfgDelay cfg)
    `raceM` limitM (cfgLimit cfg)
    `raceM` statusM (cfgStatus cfg)
