{-# LANGUAGE RecordWildCards #-}
module Chat where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Char
import qualified Data.Map as M
import Network
import System.IO

type ClientName = String

data Client = Client
  { clientName     :: ClientName
  , clientHandle   :: Handle
  , clientKicked   :: TVar (Maybe String)
  , clientSendChan :: TChan Message
  }

data Message =
    Notice String
  | Tell ClientName String
  | Broadcast ClientName String
  | Command String
  deriving (Show)

newClient :: ClientName -> Handle -> STM Client
newClient name h = do
  kicked <- newTVar Nothing
  chan   <- newTChan
  return $ Client name h kicked chan

sendMessage :: Client -> Message -> STM ()
sendMessage Client{..} msg =
  writeTChan clientSendChan msg

data Server = Server
  { serverClients :: TVar (M.Map ClientName Client)
  }

newServer :: IO Server
newServer = do
  c <- newTVarIO M.empty
  return $ Server c

broadcast :: Server -> Message -> STM ()
broadcast Server{..} msg = do
  clients <- readTVar serverClients
  mapM_ (\ client -> sendMessage client msg) (M.elems clients)

sendToName :: Server -> ClientName -> Message -> STM Bool
sendToName server@Server{..} name msg = do
  clients <- readTVar serverClients
  case M.lookup name clients of
    Nothing -> return False
    Just client -> sendMessage client msg >> return True

run :: IO ()
run = do
  server <- newServer
  let port = 44444
  s <- listenOn (PortNumber port)
  putStrLn $ "Listening on port " ++ show port
  forever $ do
    (h, chost, cport) <- accept s
    putStrLn $ "Accepted connection from " ++ show chost ++ ":" ++ show cport
    forkFinally (talk h server) (const $ hClose h)

checkAddClient :: Server -> ClientName -> Handle -> IO (Maybe Client)
checkAddClient server@Server{..} name handle = atomically $ do
  clients <- readTVar serverClients
  if M.member name clients
    then return Nothing
    else do
      client <- newClient name handle
      writeTVar serverClients $ M.insert name client clients
      broadcast server $ Notice (name ++ " has connected")
      return (Just client)

removeClient :: Server -> ClientName -> IO ()
removeClient server@Server{..} name = atomically $ do
  modifyTVar' serverClients $ M.delete name
  broadcast server $ Notice (name ++ " has disconnected")

talk :: Handle -> Server -> IO ()
talk h server@Server{..} = do
  hSetNewlineMode h universalNewlineMode
  hSetBuffering h LineBuffering
  negotiateName
  where
    negotiateName = do
      hPutStrLn h "What is your name?"
      name <- hGetLine h
      unless (validName name) negotiateName
      ok <- checkAddClient server name h -- suspicious pattern part 1
      case ok of
        Nothing -> do
          hPutStrLn h $ "Name is in use, please choose another ..."
          negotiateName
        Just client -> do
          runClient server client `finally` removeClient server name -- suspicious pattern part 2

-- We will talk about asynchronous exceptions in more detail later.

validName :: String -> Bool
validName x = not (null x) && all isLetter x

runClient :: Server -> Client -> IO ()
runClient server@Server{..} client@Client{..} =
  void $ race receive serve
  where
    receive :: IO ()
    receive = forever $ do
      msg <- hGetLine clientHandle
      atomically $ sendMessage client (Command msg)

    serve :: IO ()
    serve = join $ atomically $ do
      kicked <- readTVar clientKicked
      case kicked of
        Just reason ->
          return $ hPutStrLn clientHandle $ "You have been kicked: " ++ reason
        Nothing -> do
          msg <- readTChan clientSendChan
          return $ handleMessage server client msg serve

handleMessage :: Server -> Client -> Message -> IO () -> IO ()
handleMessage server client@Client{..} msg cont =
  case msg of
    Notice msg         -> output $ "*** " ++ msg
    Tell name msg      -> output $ "*" ++ name ++ "*: " ++ msg
    Broadcast name msg -> output $ "<" ++ name ++ ">: " ++ msg
    Command msg        ->
      case words msg of
        "/kick" : who : [] -> atomically (kick server client who) >> cont
        "/tell" : who : what -> tell server client who (unwords what) >> cont
        "/quit" : [] -> return ()
        _ -> atomically (broadcast server (Broadcast clientName msg)) >> cont
  where
    output :: String -> IO ()
    output txt =
      hPutStrLn clientHandle txt >> cont

kick :: Server -> Client -> ClientName -> STM ()
kick server@Server{..} client who = do
  clients <- readTVar serverClients
  case M.lookup who clients of
    Nothing -> sendMessage client (Notice $ who ++ " is not connected.")
    Just victim -> do
      writeTVar (clientKicked victim) $ Just $ "by " ++ clientName client
      sendMessage client (Notice $ "you kicked " ++ who)

tell :: Server -> Client -> ClientName -> String -> IO ()
tell server@Server{..} client@Client{..} who msg = do
  ok <- atomically $ sendToName server who (Tell clientName msg)
  unless ok $ atomically $ sendMessage client (Notice $ who ++ " is not connected.")
