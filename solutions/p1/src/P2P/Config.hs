{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

{-|
Module      : P2P.Config
Description : Configuration of the peer-to-peer protocol.
Copyright   : (c) Lars Brünjes, 2019
License     : MIT
Maintainer  : lars.bruenjes@iohk.io
Stability   : experimental
Portability : portable

This module contains types and functions for configuring the peer-to-peer protocol.
-}

module P2P.Config
    ( Config (..)
    , parseConfig
    ) where

import           Data.Maybe                   (fromMaybe, mapMaybe)
import           Network                      (PortNumber)
import           Options.Generic
import           Text.ParserCombinators.ReadP
import           Text.Read                    (readMaybe)

import           P2P.Core

data Args w = Args
    { port   :: w ::: Int          <?> "Port number"
    , tmin   :: w ::: Maybe Double <?> "Minimal delay for transaction generation (seconds), default 10"
    , tmax   :: w ::: Maybe Double <?> "Maximal delay for transaction generation (seconds), default 120"
    , delay  :: w ::: Maybe Double <?> "Delay for peer discovery (seconds), default 10"
    , peer   :: w ::: [String]     <?> "Peers"
    , limit  :: w ::: Maybe Int    <?> "Maximal number of connected peers, default 2"
    , status :: w ::: Maybe Double <?> "Delay between status updates (seconds), default 10"
    } deriving (Generic)

instance ParseRecord (Args Wrapped)
deriving instance Show (Args Unwrapped)

-- | Configuration data for the peer-to-peer protocol.
data Config = Config
    { cfgPort   :: PortNumber   -- ^ The port number to start the server on.
    , cfgTMin   :: Microseconds -- ^ Minimal waiting time before generating a new transaction.
    , cfgTMax   :: Microseconds -- ^ Maximal waiting time before generating a new transaction.
    , cfgDelay  :: Microseconds -- ^ Waiting time for peer discovery.
    , cfgPeers  :: [Peer]       -- ^ Initially known peers.
    , cfgLimit  :: Int          -- ^ Maximal number of connected peers.
    , cfgStatus :: Microseconds -- ^ Delay between status updates.
    } deriving Show

seconds2microseconds :: Double -> Microseconds
seconds2microseconds s = ceiling $ s * 1000000

args2Config :: Args Unwrapped -> Config
args2Config args = Config
    { cfgPort   = fromIntegral         $ port args
    , cfgTMin   = seconds2microseconds $ fromMaybe  10 $ tmin   args
    , cfgTMax   = seconds2microseconds $ fromMaybe 120 $ tmax   args
    , cfgDelay  = seconds2microseconds $ fromMaybe  10 $ delay  args
    , cfgPeers  = mapMaybe readPeer                    $ peer   args
    , cfgLimit  =                        fromMaybe   2 $ limit  args
    , cfgStatus = seconds2microseconds $ fromMaybe  10 $ status args
    }

-- | Tries to parse all configuration data from command line arguments.
parseConfig :: IO Config
parseConfig = args2Config <$> unwrapRecord "p2p"

digitP :: ReadP Char
digitP = satisfy (`elem` ("0123456789" :: String))

portNumberP :: ReadP PortNumber
portNumberP = do
    s <- many1 digitP
    case readMaybe s of
        Just port' -> return port'
        Nothing    -> pfail

peerP :: ReadP Peer
peerP = Peer <$> many1 get <*> (char ':' *> portNumberP)

readPeer :: String -> Maybe Peer
readPeer s = case readP_to_S (peerP <* eof) s of
    [(peer', "")] -> Just peer'
    _             -> Nothing
