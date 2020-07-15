{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server where

import Control.Monad.Logger
import Network.Wai.Handler.Warp
  ( run
  )
import Network.Wai.Middleware.Cors
  ( simpleCors
  )
import Network.Wai.Middleware.Static
  ( addBase
  , policy
  , staticPolicy
  )
import Network.WebSockets
  ( receiveData
  , sendTextData
  , withPingThread
  )
import Servant.API
  ( (:<|>)(..)
  )
import Servant.Server
  ( ServerError(..)
  , err400
  , hoistServer
  , serve
  )

import Control.Exception (ErrorCall)
import qualified Control.Exception.Safe as Exception (Handler(..))
import Control.Exception.Safe (catches, throwM)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (runReaderT)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text as T

import App

ws :: AppServer ApiWebSocket
ws c = errorHandler $ do
  _ <- liftIO . withPingThread c 45 (return ()) . forever $ do
    bs <- receiveData c
    if BS.null bs
      then return ()
      else case BS.head bs of
        0x0D -> sendTextData c ("\r\n$ " :: BS.ByteString)
        0x03 -> sendTextData c ("\r\n$ " :: BS.ByteString)
        0x7F -> sendTextData c ("\b \b" :: BS.ByteString)
        b -> sendTextData c $ BS.pack [b]
  return ()

errorHandler :: AppHandler a -> AppHandler a
errorHandler = flip catches
  [ Exception.Handler $ \(e :: ServerError) -> do
      logErrorN $ T.pack $ show e
      throwM e
  , Exception.Handler $ \(e :: ErrorCall) -> do
      logErrorN $ T.pack $ show e
      throwM err400 { errBody = BLC.pack $ show e }
  ]

startServer :: IO ()
startServer = do
  let runLogger :: MonadIO m => LoggingT m a -> m a
      runLogger = runStderrLoggingT . filterLogger (\_ level -> level > LevelDebug)
  let arg = AppArg (AppConfig True)
  run 8080 $ middleware $ serve api $ hoistServer api (runLogger . flip runReaderT arg)
    $ ws
 where
  middleware = simpleCors . midPolicy
  midPolicy = staticPolicy $ indexPolicy <> addBase "./html"
  indexPolicy = policy $ Just . \b -> if null b || last b == '/' then b <> "index.html" else b

serverMain :: IO ()
serverMain = startServer
