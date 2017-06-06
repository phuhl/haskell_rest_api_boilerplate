module RestServer.Persistent.Postgresql.Config
  ( getCConnectionConfig
  , getCPoolSize
  , DBConnectionConfig(..)
  ) where

import           RestServer.ConfigReader
import           RestServer.Environment

data DBConnectionConfig = DBConnectionConfig
  {
    getHost :: String
  , getDBName :: String
  , getUser :: String
  , getPassword :: String
  , getDBPort :: String
  }

getCPoolSize :: ConfigFile -> Environment -> Int
getCPoolSize cf Production = readConfig 8 cf "db" "productionPoolsize"
getCPoolSize _ _ = 1

getCConnectionConfig :: ConfigFile -> DBConnectionConfig
getCConnectionConfig configFile = DBConnectionConfig
  (readConfig "localhost" configFile "db" "host")
  (readConfig "myDB" configFile "db" "database")
  (readConfig "root" configFile "db" "user")
  (readConfig "123456" configFile "db" "password")
  (readConfig "5432" configFile "db" "port" )
