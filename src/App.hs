{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module App where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Logger (LoggingT)
import Control.Monad.Reader (MonadReader(..), ReaderT(..))
import Servant
import Servant.API.WebSocket

data AppConfig = AppConfig
  { _appConfigDebug :: Bool
  }
  deriving (Show)

data AppArg = AppArg
  { _appArgConfig :: AppConfig
  }

type AppHandler = ReaderT AppArg (LoggingT Handler)
type AppServer api = ServerT api AppHandler

type ApiWebSocket = WebSocket
type Api = "ws" :> ApiWebSocket

api :: Proxy Api
api = Proxy

$(makeLenses ''AppConfig)
$(makeLenses ''AppArg)

appArgConfigDebug :: Lens' AppArg Bool
appArgConfigDebug = appArgConfig . appConfigDebug
