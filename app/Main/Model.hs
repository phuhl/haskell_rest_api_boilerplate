{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Main.Model where

import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH


share [ mkPersist sqlSettings
      , mkDeleteCascade sqlSettings
      , mkMigrate "myMigration"]
  [persistLowerCase|
Test
    value String
    deriving Show Eq
|]
