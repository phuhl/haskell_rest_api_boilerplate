
module RestServer.Config
    (
      getCPort
    ) where


import           RestServer.ConfigReader
import           Network.Wai.Handler.Warp (Port)

getCPort :: ConfigFile -> Port
getCPort configFile = readConfig 8081 configFile "server" "port"
