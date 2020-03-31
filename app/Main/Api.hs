{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main.Api where

import RestServer.Persistent.Postgresql (Config(..))
import qualified RestServer as RS

import Data.Text

import Servant              (ServerT, Proxy(..))
import Servant.API

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
