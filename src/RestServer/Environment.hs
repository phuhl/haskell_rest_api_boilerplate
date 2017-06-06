module RestServer.Environment (Environment(..)) where

data Environment
  = Development
  | Test
  | Production
  deriving (Eq, Show, Read)
