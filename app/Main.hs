{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}


module Main where

import qualified RestServer as RS
import RestServer.Config (getCPort)
import RestServer.ConfigReader (readConfigFile)
import RestServer.Persistent.Postgresql (Config(..)
                                        , createDbConnection)
import RestServer.Persistent.Postgresql.Config (getCConnectionConfig
                                                , getCPoolSize)
import Control.Monad.Except (ExceptT)
import Servant              (ServerT, Proxy(..))
import Servant.API

import Data.Text

import Database.Persist.Postgresql (ConnectionPool)

type API =
  "v1" :> "test1" :> Get '[JSON] Text
  :<|> "v1" :> "test2" :> Get '[JSON] Text


type App a = RS.App Config a

api :: ServerT API (RS.App Config)
api = test1 :<|> test2

apiP :: Proxy API
apiP = Proxy

test1 :: App Text
test1 = return "test1"

test2 :: App Text
test2 = return "test2"

main :: IO ()
main = do
  configFile <- readConfigFile "config.ini"
  config <- Config <$> createDbConnection
    (getCConnectionConfig configFile)
    RS.Test
    (getCPoolSize configFile)
    Nothing
  RS.createRestServer (getCPort configFile) RS.Test config apiP api
