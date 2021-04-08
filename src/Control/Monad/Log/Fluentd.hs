{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language TypeFamilies #-}
{-# options_ghc -Wno-unused-imports #-}
module Control.Monad.Log.Fluentd (fluentdLogging) where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Functor (void)
import qualified Data.List.NonEmpty as NE (NonEmpty, nonEmpty, toList)
import Data.Proxy (Proxy(..))

-- bytestring
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Lazy as LBS (toStrict, fromStrict)
import qualified Data.ByteString.Char8 as BS (pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as LBS (toStrict, fromStrict)
-- exceptions
import Control.Monad.Catch (MonadCatch(..), MonadMask(..))
-- http-client
import qualified Network.HTTP.Client as H (Manager, ManagerSettings, newManager, defaultManagerSettings, managerResponseTimeout, responseTimeoutMicro, responseTimeoutNone)
-- http-client-tls
import qualified Network.HTTP.Client.TLS as H (tlsManagerSettings)
-- logging-effect
import Control.Monad.Log (withBatchedHandler, BatchingOptions(..), defaultBatchingOptions, WithSeverity(..), Handler, runLoggingT)
import qualified Control.Monad.Log as L (Severity(..))
-- msgpack-binary
import qualified Data.MessagePack as Msg (MessagePack(..), pack)
-- req
import Network.HTTP.Req (MonadHttp(..), Req, runReq, withReqManager, defaultHttpConfig, req, reqBr, defaultHttpConfig, httpConfigAltManager, bsResponse, jsonResponse, ignoreResponse, GET(..), POST(..), ReqBodyLbs(..), ReqBodyJson(..), ReqBodyUrlEnc(..), Url, https, (/:), responseBody, (=:), Option, Scheme(..), basicAuth, oAuth2Bearer, useHttpsURI, HttpConfig(..), HttpException(..))

-- | Logging bracket that periodically sends a batch of logs to fluentd, encoding them in @msgpack@ format first.
fluentdLogging :: (Msg.MessagePack a, MonadIO m, MonadMask m) =>
                  BatchingOptions
               -> Url scheme -- ^ fluentd server URI (either http or https)
               -> (Handler m a -> m b) -- ^ User program runs in here, via e.g. 'runLoggingT' or similar
               -> m b
fluentdLogging opts uri k = withTLS $ \tlscfg -> withBatchedHandler opts (flush tlscfg) k
  where
    flush tlscfg mq = runReq tlscfg $ fluentdReq uri (NE.toList mq)


withTLS :: MonadIO m =>
           (HttpConfig -> m a) -- ^ all calls to 'runReq' should happen in here
        -> m a
withTLS f = do
  mgr <- liftIO $ H.newManager H.tlsManagerSettings
  let cfg = defaultHttpConfig{ httpConfigAltManager = Just mgr }
  f cfg

fluentdReq :: (Msg.MessagePack a, MonadHttp m) =>
              Url scheme -- ^ server URI
           -> a -- ^ message payload
           -> m ()
fluentdReq u dat = do
  void $ req POST u (ReqBodyLbs lbs) ignoreResponse mempty
  where
    lbs = "msgpack=" <> Msg.pack dat
