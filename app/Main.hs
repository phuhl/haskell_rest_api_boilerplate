{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}


module Main where

import qualified RestServer as RS

import           Control.Monad.Except        (ExceptT)
import           Servant (ServerT, Proxy(..))
import           Servant.API

import           Data.Text

data Config = Config {}

type API =
  "v1" :> "test1" :> Get '[JSON] Text
  :<|> "v1" :> "test2" :> Get '[JSON] Text


api :: ServerT API (RS.App Config)
api = test1 :<|> test2

apiP :: Proxy API
apiP = Proxy

test1 :: (RS.App Config) Text
test1 = return "test1"

test2 :: (RS.App Config) Text
test2 = return "test2"

main :: IO ()
main = RS.createRestServer 8080 (Config) apiP api
