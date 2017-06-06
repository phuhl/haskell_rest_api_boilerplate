{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}

module RestServer
    (
      createRestServer,
      App(..),
      Environment(..)
    ) where

import RestServer.Environment

import Control.Monad.Reader (MonadIO, MonadReader, ReaderT
                            , runReaderT, asks, liftIO)
import Control.Monad.Except (ExceptT, MonadError)
import Servant              (Server, ServerT, HasServer
                            , ServantErr, Proxy, serve)
import Servant.Utils.Enter  (Enter, (:~>)(..), enter)
import Network.Wai (Middleware(..))
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Network.Wai.Handler.Warp (run,Port)



newtype App config a
    = App
    { runApp :: ReaderT config (ExceptT ServantErr IO) a
    } deriving ( Functor, Applicative, Monad, MonadReader config,
                 MonadError ServantErr, MonadIO)


convertApp :: config -> (App config) :~> ExceptT ServantErr IO
convertApp cfg = Nat (flip runReaderT cfg . runApp)

setLogger :: Environment -> Middleware
setLogger Test = id
setLogger Development = logStdoutDev
setLogger Production = logStdout

createRestServer :: ((Enter
                      (ServerT api (App config))
                      ((App config) :~> ExceptT ServantErr IO)
                      (Server api)
                     )
                    , (HasServer api '[]))
  => Port -> Environment -> config -> Proxy api
  -> ServerT api (App config)
  -> IO ()
createRestServer port environment config proxy serverApi =
  run port
  $ setLogger environment
  $ serve proxy
  $ enter (convertApp config) (serverApi)
