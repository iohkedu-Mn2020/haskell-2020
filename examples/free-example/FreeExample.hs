{-# LANGUAGE DeriveFunctor #-}

module FreeExample where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Free
import Prelude            hiding (read)

-- want Process monad
-- operations:
--
-- ch - type of communication channels
--
-- fork :: Process () -> Process ()
-- newChannel :: Process Channel
-- write :: Channel -> String -> Process ()
-- read  :: Channel -> Process String
-- trace :: String -> Process ()

data ProcessOp ch a =
      Fork (Process ch ()) a -- fork a concurrent process
    | NewChannel (ch -> a)   -- create a new communication channel
    | Write ch String a      -- send a string over a channel
    | Read ch (String -> a)  -- receive a string from a channel
    | Trace String a         -- trace a log-message
    deriving Functor

type Process ch = Free (ProcessOp ch)

processIO :: Process (TChan String) a -> IO a
processIO proc = do
    lock <- newMVar ()
    processIO' lock proc
  where
    processIO' :: MVar () -> Process (TChan String) a -> IO a
    processIO' lock = go
      where
        go :: Process (TChan String) a -> IO a
        go (Pure a)              = return a
        go (Free (Fork p k))     = forkIO (go p)                      >>  go k
        go (Free (NewChannel k)) = newTChanIO                         >>= go . k
        go (Free (Write ch s k)) = atomically (writeTChan ch s)       >>  go k
        go (Free (Read ch k))    = atomically (readTChan ch)          >>= go . k
        go (Free (Trace s k))    = withMVar lock (const $ putStrLn s) >>  go k

fork :: Process ch () -> Process ch ()
fork p = Free $ Fork p $ return ()

newChannel :: Process ch ch
newChannel = Free $ NewChannel return

write :: ch -> String -> Process ch ()
write ch s = Free $ Write ch s $ return ()

read :: ch -> Process ch String
read ch = Free $ Read ch return

trace :: String -> Process ch ()
trace s = Free $ Trace s $ return ()

pingPong :: Process ch a
pingPong = do
    ch_ping_to_pong <- newChannel
    ch_pong_to_ping <- newChannel
    fork (pong ch_ping_to_pong ch_pong_to_ping)
    ping ch_ping_to_pong ch_pong_to_ping
  where
    ping :: ch -> ch -> Process ch a
    ping ch_ping_to_pong ch_pong_to_ping = forever $ do
        write ch_ping_to_pong "PING"
        trace "sent PING"
        s <- read ch_pong_to_ping
        trace $ "received: " ++ s

    pong :: ch -> ch -> Process ch a
    pong ch_ping_to_pong ch_pong_to_ping = forever $ do
        s <- read ch_ping_to_pong
        trace $ "reveived: " ++ s
        write ch_pong_to_ping "PONG"
        trace "sent PONG"
