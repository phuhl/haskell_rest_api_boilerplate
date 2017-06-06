
module RestServer.ConfigReader
    (
      readConfig
    , readConfigFile
    , ConfigFile(..)
    ) where


import qualified Data.ConfigFile as CF
import qualified Control.Monad.Error as Error

type ConfigFile = CF.ConfigParser

readConfig :: CF.Get_C a
  => a -> ConfigFile -> String -> String -> a
readConfig defaultVal conf sec opt = fromEither defaultVal
  $ fromEither (Right defaultVal)
  $ Error.runErrorT $ CF.get conf sec opt

readConfigFile :: String -> IO ConfigFile
readConfigFile path = do
  c <- Error.catchError (CF.readfile CF.emptyCP path)
    (\e ->  do
        putStrLn $ show e
        return $ return CF.emptyCP)
  let c1 = fromEither CF.emptyCP c
  return c1

fromEither :: a -> Either b a -> a
fromEither a e = case e of
  Left _ -> a
  Right x -> x
