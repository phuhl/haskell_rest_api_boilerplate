{-# LANGUAGE FlexibleContexts #-}
module RestServer.Persistent.MongoDB
    (
      runDb
    , createDbConnection
    , defaultPoolSize
    , DBConnectionConfig(..)
    , DBConfig(..)
    , Config(..)
    ) where

import qualified  RestServer.Persistent as RSP ()
import            RestServer.Environment
import            RestServer.Persistent.Postgresql.Config

import qualified Data.ByteString.Char8 as BS

import Control.Monad.Reader (MonadIO, MonadReader, asks, liftIO)
import Control.Monad.Logger    (runStdoutLoggingT, runNoLoggingT)
import Database.Persist.Sql (SqlBackend)
import Database.Persist.Postgresql (runSqlPool, runMigration
                                   , createPostgresqlPool
                                   , SqlPersistT , Migration
                                   , ConnectionString
                                   , ConnectionPool)


class DBConfig a where
  dbconfPool :: a -> ConnectionPool

data Config = Config { confPool :: ConnectionPool}

instance DBConfig Config where
  dbconfPool (Config a) = a

createDbConnection :: DBConnectionConfig -> Environment
  -> (Environment -> Int)
  -> Maybe (Migration)
  -> IO ConnectionPool
createDbConnection config environment poolSize mMigration = do
  pool <- makePool environment (genConString config) poolSize
  case mMigration of
    Nothing -> return ()
    (Just migration) ->
      runSqlPool (runMigration migration) pool
  return pool

runDb :: (DBConfig a, MonadReader a m, MonadIO m)
  => SqlPersistT IO b -> m b
runDb query = do
  pool <- asks dbconfPool
  liftIO $ runSqlPool query $ pool


defaultPoolSize :: Environment -> Int
defaultPoolSize Production = 8
defaultPoolSize _ = 1

genConString :: DBConnectionConfig -> Environment -> ConnectionString
genConString c d =
  BS.pack $ concat $ zipWith (++)
  ["host=", (if (d == Test) then " dbname=test" else " dbname="),
    " user=", " password=", " port="]
  ([getHost, getDBName, getUser, getPassword, getDBPort] <*> [c])

makePool :: Environment -> (Environment -> ConnectionString)
  -> (Environment -> Int) -> IO ConnectionPool
makePool e connStr poolSize =
  case e of
    Test -> runNoLoggingT (createPostgresqlPool
                            (connStr e) (poolSize e))
    otherwise -> runStdoutLoggingT (createPostgresqlPool
                                    (connStr e) (poolSize e))

