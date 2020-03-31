module Main where

import Main.Api (apiP, api)
import Main.Model (myMigration)

import qualified RestServer as RS
import RestServer.Config (getCPort)
import RestServer.ConfigReader (readConfigFile)
import RestServer.Persistent.Postgresql (Config(..)
                                        , createDbConnection)
import RestServer.Persistent.Postgresql.Config (getCConnectionConfig
                                                , getCPoolSize)

import Database.Persist.Postgresql (ConnectionPool)


main :: IO ()
main = do
  configFile <- readConfigFile "config.ini"
  config <- Config <$> createDbConnection
    (getCConnectionConfig configFile)
    RS.Test
    (getCPoolSize configFile)
    (Just myMigration)
  RS.createRestServer (getCPort configFile) RS.Test config apiP api
