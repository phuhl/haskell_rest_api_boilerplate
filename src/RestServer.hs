{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}

module RestServer
    (
      createRestServer,
      App(..)
    ) where


import Control.Monad.Reader (MonadIO, MonadReader, ReaderT
                            , runReaderT, asks, liftIO)
import Control.Monad.Except (ExceptT, MonadError)
import Servant              (Server, ServerT, HasServer
                            , ServantErr, Proxy, serve)
import Servant.Utils.Enter  (Enter, (:~>)(..), enter)
import Network.Wai.Handler.Warp (run,Port)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)


newtype App config a
    = App
    { runApp :: ReaderT config (ExceptT ServantErr IO) a
    } deriving ( Functor, Applicative, Monad, MonadReader config,
                 MonadError ServantErr, MonadIO)


convertApp :: config -> (App config) :~> ExceptT ServantErr IO
convertApp cfg = Nat (flip runReaderT cfg . runApp)

createRestServer :: ((Enter
                      (ServerT api (App config))
                      ((App config) :~> ExceptT ServantErr IO)
                      (Server api)
                     )
                    , (HasServer api '[]))
  => Port -> config -> Proxy api -> ServerT api (App config)
  -> IO ()
createRestServer port config proxy serverApi =
  run port
  $ logStdout
  $ serve proxy
  $ enter (convertApp config) (serverApi)


{-startServer :: Environment -> IO ()
startServer e = do
  (pool, logger, config) <- getAllConfigs e
  runSqlPool (runMigration usersMigration) pool
  let port = confPort config
  runStdoutLoggingT $ do logInfoN $ Text.pack $ "Listening on port " ++ show port
  run port $ logger $ app config
-}
