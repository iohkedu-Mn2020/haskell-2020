{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Hello
    ( userApp
    , User (..)
    ) where

import Conduit                 (ResourceT)
import Control.Monad.Logger    (NoLoggingT)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson              (FromJSON, ToJSON)
import Data.Text               (Text, pack)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Generics            (Generic)
import Servant
import Servant.Docs
-- import Servant.JS

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    name String
    age Int Maybe
    deriving Show
|]

deriving stock instance Generic User
deriving anyclass instance ToJSON User
deriving anyclass instance FromJSON User

newtype M a = M (ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a)
    deriving newtype (Functor, Applicative, Monad, MonadReader SqlBackend, MonadIO)

fromMtoHandler :: Text -> M a -> Handler a
fromMtoHandler cs (M m) = liftIO $ runSqlite cs $ do
    runMigration migrateAll
    m

type UserApi =      "users" :> Get '[JSON] [User]
               :<|> "user"  :> Capture "userId" UserId :> Get '[JSON] User
               :<|> "user"  :> Capture "userId" UserId :> Delete '[JSON] NoContent
               :<|> "user"  :> ReqBody '[JSON] User :> Post '[JSON] UserId

instance ToCapture (Capture "userId" Int) where
  toCapture _ = DocCapture "userId" "the user id"

instance ToSample Int where
  toSamples _ = singleSample 42

instance ToSample User where
  toSamples _ = singleSample $ User "Lars" (Just 48)

userApi :: Proxy UserApi
userApi = Proxy

handleUsers :: M [User]
handleUsers = do
    xs <- M $ selectList [] []
    return $ map entityVal xs
    {-
    v <- ask
    liftIO $ readMVar v
-}

handleGetUser :: UserId -> M User
handleGetUser uid = do
    m <- M $ getEntity uid
    case m of
        Just e  -> return $ entityVal e
        Nothing -> liftIO $ ioError $ userError "key not found"
{-
    users <- handleUsers
    case filter (\u -> userId u == uid) users of
        (u : _) -> return u
        []      -> throwError err404
-}

handleDeleteUser :: UserId -> M NoContent
handleDeleteUser uid = do
    M $ delete uid
    return NoContent

handlePostUser :: User -> M UserId
handlePostUser user = M $ insert user

userServer :: ServerT UserApi M
userServer = handleUsers :<|> handleGetUser :<|> handleDeleteUser :<|> handlePostUser

userApp :: String -> Application
userApp cs = serve userApi $ hoistServer userApi (fromMtoHandler $ pack cs) userServer
