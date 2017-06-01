
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RestServer
    (
      createRestServer
    ) where


import           Control.Monad.Reader        (MonadIO, MonadReader,
                                              ReaderT, runReaderT, asks, liftIO)
import           Control.Monad.Except        (ExceptT, MonadError)

import           Servant
import           Servant.API
import           Network.Wai.Handler.Warp (run,Port)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)

newtype App config a
    = App
    { runApp :: ReaderT config (ExceptT ServantErr IO) a
    } deriving ( Functor, Applicative, Monad, MonadReader config,
                 MonadError ServantErr, MonadIO)


convertApp :: config -> (App config) :~> ExceptT ServantErr IO
convertApp cfg = Nat (flip runReaderT cfg . runApp)

appToServer :: config -> ServerT api (App config) -> Server api
appToServer config serverApi = enter (convertApp config) serverApi

api :: Proxy api
api = Proxy

app :: config -> ServerT api (App config) -> Application
app config serverApi = serve api $ appToServer config serverApi

createRestServer :: Port -> ServerT api (App config) -> config -> IO ()
createRestServer port serverApi config = do
  run port $ logStdout $ app config serverApi



{-startServer :: Environment -> IO ()
startServer e = do
  (pool, logger, config) <- getAllConfigs e
  runSqlPool (runMigration usersMigration) pool
  let port = confPort config
  runStdoutLoggingT $ do logInfoN $ Text.pack $ "Listening on port " ++ show port
  run port $ logger $ app config
-}