{-|
Module      : P2P.Server
Description : Transaction generator.
Copyright   : (c) Lars Brünjes, 2019
License     : MIT
Maintainer  : lars.bruenjes@iohk.io
Stability   : experimental
Portability : portable

This module defines an agent for transaction generation.
-}

module P2P.TxGenerator
    ( txGeneratorM
    ) where

import           Control.Concurrent     (threadDelay)
import           Control.Monad          (forever)
import           Control.Monad.IO.Class (liftIO)
import           P2P.Core
import           System.Random          (randomRIO)

-- | Agent for transaction generation.
txGeneratorM :: Microseconds -- ^ Minimal wait until generating the next transaction.
             -> Microseconds -- ^ Maximal wait until generating the next transaction.
             -> M ()
txGeneratorM tmin tmax
    | tmin > tmax = return ()
    | otherwise   = forever $ do
        t <- liftIO $ randomRIO (tmin, tmax)
        liftIO $ threadDelay t
        tx <- createTxM
        logM $ "created new transaction " ++ show tx
        sendAllPeersM $ Newtx tx
