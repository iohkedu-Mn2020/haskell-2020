{-|
Module      : P2P.Status
Description : Status display
Copyright   : (c) Lars Brünjes, 2019
License     : MIT
Maintainer  : lars.bruenjes@iohk.io
Stability   : experimental
Portability : portable

This module defines an agent displaying the node status in regular intervals.
-}

module P2P.Status
    ( statusM
    ) where

import           P2P.Core

import           Control.Concurrent     (threadDelay)
import           Control.Monad          (forever)
import           Control.Monad.IO.Class (liftIO)
import           Data.List              (intercalate)

-- | Agent to display the node status in regular intervals.
statusM :: Microseconds -- ^ Interval for status display.
        -> M ()
statusM status = forever $ do
    (Peer h p, ps, tx) <- stateM
    logM $ intercalate "\n" $ [ ""
                              , "Status:"
                              , "host:              " ++ h
                              , "port:              " ++ show p
                              , "transaction:       " ++ show tx
                              , "connected peers:"
                              ] ++
                              [ show p' | (p', b) <- ps, b] ++
                              [ "disconnected peers:" ] ++
                              [ show p' | (p', b) <- ps, not b] ++
                              [ "" ]
    liftIO $ threadDelay status
