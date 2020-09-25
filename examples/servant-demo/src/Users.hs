{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Users
    ( runUsers
    , writeUsersJS
    ) where

import           Control.Concurrent       (MVar, newMVar, putMVar, readMVar,
                                           takeMVar)
import           Control.Monad.Except     (MonadError (..))
import           Control.Monad.Reader     (MonadIO (..), MonadReader (..),
                                           ReaderT (..))
import           Data.Aeson               (FromJSON, ToJSON)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           GHC.Generics             (Generic)
import           Network.Wai.Handler.Warp (Port, run)
import           Servant
import           Servant.JS               (jsForAPI, vanillaJS)

data User a = User
    { uId        :: a
    , uFirstName :: Text
    , uLastName  :: Text
    , uEMail     :: Text
    , uAge       :: Int
    } deriving (Show, Read, Eq, Ord, Generic, Functor)

instance FromJSON a => FromJSON (User a)
instance ToJSON a => ToJSON (User a)

findUser :: Int -> [User Int] -> Maybe (User Int)
findUser _ [] = Nothing
findUser i (u : us)
    | uId u == i = Just u
    | otherwise  = findUser i us

newtype M a = M (ReaderT (MVar [User Int]) Handler a)
    deriving (Functor, Applicative, Monad, MonadReader (MVar [User Int]), MonadIO, MonadError ServerError)

mToHandler :: MVar [User Int] -> M a -> Handler a
mToHandler v (M m) = runReaderT m v

type UsersApi =      "users" :> Get '[JSON] [User Int]
                :<|> "user"  :> Capture "id" Int :> Get '[JSON] (User Int)
                :<|> "user"  :> Capture "id" Int :> Delete '[JSON] NoContent
                :<|> "user"  :> ReqBody '[JSON] (User ()) :> Post '[JSON] (User Int)

usersApi :: Proxy (UsersApi :<|> Raw)
usersApi = Proxy

usersHandler :: M [User Int]
usersHandler = do
    v <- ask
    liftIO $ readMVar v

userGetHandler :: Int -> M (User Int)
userGetHandler i = do
    us <- usersHandler
    case findUser i us of
        Nothing -> throwError err404
        Just u  -> return u

userDeleteHandler :: Int -> M NoContent
userDeleteHandler i = do
    v  <- ask
    us <- liftIO $ takeMVar v
    let us' = filter (\u -> uId u /= i) us
    liftIO $ putMVar v us'
    return NoContent

userPostHandler :: User () -> M (User Int)
userPostHandler u = do
    v  <- ask
    us <- liftIO $ takeMVar v
    let ids = [uId w | w <- us]
        i   = head $ filter (`notElem` ids) [1 ..]
        u'  = i <$ u
        us' = u' : us
    liftIO $ putMVar v us'
    return u'

usersServer :: ServerT (UsersApi :<|> Raw) M
usersServer = (      usersHandler
                :<|> userGetHandler
                :<|> userDeleteHandler
                :<|> userPostHandler
              ) :<|> serveDirectoryWebApp "static"

usersApp :: MVar [User Int] -> Application
usersApp v = serve usersApi $ hoistServer usersApi (mToHandler v) usersServer

runUsers :: Port -> IO ()
runUsers port = do
    v <- newMVar []
    run port $ usersApp v

writeUsersJS :: IO ()
writeUsersJS = writeFile "static/users.js" $ T.unpack $ jsForAPI (Proxy :: Proxy UsersApi) vanillaJS
