{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Cardano.TxSubmit.Web
  ( runTxSubmitServer
  ) where

import           Cardano.Api (AllegraEra, AnyCardanoEra (AnyCardanoEra),
                   AnyConsensusMode (AnyConsensusMode), AnyConsensusModeParams (..), AsType (..),
                   CardanoEra (..), Error (..), FromSomeType (..), HasTypeProxy (AsType),
                   InAnyCardanoEra (..),
                   LocalNodeConnectInfo (LocalNodeConnectInfo, localConsensusModeParams, localNodeNetworkId, localNodeSocketPath),
                   NetworkId, SerialiseAsCBOR (..), ShelleyEra, SocketPath, ToJSON, Tx, TxId (..),
                   TxInMode (TxInMode),
                   TxValidationErrorInMode (TxValidationEraMismatch, TxValidationErrorInMode),
                   consensusModeOnly, getTxBody, getTxId, submitTxToNodeLocal, toEraInMode)

import           Cardano.Binary (DecoderError (..))
import           Cardano.BM.Trace (Trace, logInfo)
import qualified Cardano.Crypto.Hash.Class as Crypto
import           Cardano.TxSubmit.Metrics (TxSubmitMetrics (..))
import           Cardano.TxSubmit.Rest.Types (WebserverConfig (..), toWarpSettings)
import qualified Cardano.TxSubmit.Rest.Web as Web
import           Cardano.TxSubmit.Types (EnvSocketError (..), RawCborDecodeError (..),
                   TxCmdError (TxCmdEraConsensusModeMismatch, TxCmdTxReadError, TxCmdTxSubmitError, TxCmdTxSubmitErrorEraMismatch),
                   TxSubmitApi, TxSubmitApiRecord (..), TxSubmitWebApiError (TxSubmitFail),
                   renderTxCmdError)
import           Cardano.TxSubmit.Util (logException)
import           Ouroboros.Consensus.Cardano.Block (EraMismatch (..))
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Client as Net.Tx

import           Control.Applicative (Applicative (pure), (<$>))
import           Control.Monad (Functor (fmap), Monad (return), (=<<))
import           Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import           Control.Monad.IO.Class (MonadIO (liftIO), liftIO)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither,
                   hoistMaybe, left, newExceptT)
import           Data.Aeson (ToJSON (..))
import qualified Data.Aeson as Aeson
import           Data.Bifunctor (first, second)
import qualified Data.ByteString as BS
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Char as Char
import           Data.Either (Either (..), partitionEithers)
import           Data.Function (($), (.))
import qualified Data.List as L
import qualified Data.List.NonEmpty as NEL
import           Data.Maybe (listToMaybe, maybe)
import           Data.Proxy (Proxy (..))
import           Data.Semigroup (Semigroup ((<>)))
import           Data.String (String)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import           System.Environment (lookupEnv)
import qualified System.IO as IO
import           System.IO (IO)
import qualified System.Metrics.Prometheus.Metric.Gauge as Gauge
import           Text.Show (Show (show))

import qualified Servant
import           Servant (Application, Handler, ServerError (..), err400, throwError)
import           Servant.API.Generic (toServant)
import           Servant.Server.Generic (AsServerT)

runTxSubmitServer
  :: Trace IO Text
  -> TxSubmitMetrics
  -> WebserverConfig
  -> AnyConsensusModeParams
  -> NetworkId
  -> SocketPath
  -> IO ()
runTxSubmitServer trace metrics webserverConfig protocol networkId socketPath = do
  logException trace "tx-submit-webapi." $
    Web.runSettings trace (toWarpSettings webserverConfig) $ txSubmitApp trace metrics protocol networkId socketPath
  logInfo trace "txSubmitApp: exiting"

txSubmitApp
  :: Trace IO Text
  -> TxSubmitMetrics
  -> AnyConsensusModeParams
  -> NetworkId
  -> SocketPath
  -> Application
txSubmitApp trace metrics connectInfo networkId socketPath =
    Servant.serve (Proxy :: Proxy TxSubmitApi) (toServant handlers)
  where
    handlers :: TxSubmitApiRecord (AsServerT Handler)
    handlers = TxSubmitApiRecord
      { _txSubmitPost = txSubmitPost trace metrics connectInfo networkId socketPath
      }

deserialiseOne :: forall b. ()
  => FromSomeType SerialiseAsCBOR b
  -> ByteString
  -> Either DecoderError b
deserialiseOne (FromSomeType ttoken f) bs = f <$> deserialiseFromCBOR ttoken bs

deserialiseAnyOf :: forall b. ()
  => [FromSomeType SerialiseAsCBOR b]
  -> ByteString
  -> Either RawCborDecodeError b
deserialiseAnyOf ts te = getResult . partitionEithers $ fmap (`deserialiseOne` te) ts
  where
    getResult :: ([DecoderError], [b]) -> Either RawCborDecodeError b
    getResult (dErrors, []) = Left $ RawCborDecodeError dErrors
    getResult (_, result:_) = Right result -- take the first successful decode

readByteStringTx :: ByteString -> ExceptT TxCmdError IO (InAnyCardanoEra Tx)
readByteStringTx = firstExceptT TxCmdTxReadError . hoistEither . deserialiseAnyOf
  [ FromSomeType (AsTx AsByronEra)   (InAnyCardanoEra ByronEra)
  , FromSomeType (AsTx AsShelleyEra) (InAnyCardanoEra ShelleyEra)
  , FromSomeType (AsTx AsAllegraEra) (InAnyCardanoEra AllegraEra)
  , FromSomeType (AsTx AsMaryEra)    (InAnyCardanoEra MaryEra)
  , FromSomeType (AsTx AsAlonzoEra)  (InAnyCardanoEra AlonzoEra)
  , FromSomeType (AsTx AsBabbageEra) (InAnyCardanoEra BabbageEra)
  , FromSomeType (AsTx AsConwayEra)  (InAnyCardanoEra ConwayEra)
  ]

txSubmitPost
  :: Trace IO Text
  -> TxSubmitMetrics
  -> AnyConsensusModeParams
  -> NetworkId
  -> SocketPath
  -> ByteString
  -> Handler TxId
txSubmitPost trace metrics (AnyConsensusModeParams cModeParams) networkId socketPath txBytes =
  handle $ do
    InAnyCardanoEra era tx <- readByteStringTx txBytes
    let cMode = AnyConsensusMode $ consensusModeOnly cModeParams
    eraInMode <- hoistMaybe
                   (TxCmdEraConsensusModeMismatch cMode (AnyCardanoEra era))
                   (toEraInMode era $ consensusModeOnly cModeParams)
    let txInMode = TxInMode tx eraInMode
        localNodeConnInfo = LocalNodeConnectInfo
                              { localConsensusModeParams = cModeParams
                              , localNodeNetworkId = networkId
                              , localNodeSocketPath = socketPath
                              }

    res <- liftIO $ submitTxToNodeLocal localNodeConnInfo txInMode
    case res of
      Net.Tx.SubmitSuccess -> do
        liftIO $ T.putStrLn "Transaction successfully submitted."
        return $ getTxId (getTxBody tx)
      Net.Tx.SubmitFail reason ->
        case reason of
          TxValidationErrorInMode err _eraInMode -> left . TxCmdTxSubmitError . T.pack $ show err
          TxValidationEraMismatch mismatchErr -> left $ TxCmdTxSubmitErrorEraMismatch mismatchErr
    where
      handle :: ExceptT TxCmdError IO TxId -> Handler TxId
      handle f = do
        result <- liftIO $ runExceptT f
        handleSubmitResult result

      errorResponse :: ToJSON e => e -> Handler a
      errorResponse e = throwError $ err400 { errBody = Aeson.encode e }

      -- Log relevant information, update the metrics, and return the result to
      -- the client.
      handleSubmitResult :: Either TxCmdError TxId -> Handler TxId
      handleSubmitResult res =
        case res of
          Left err -> do
            liftIO $ logInfo trace $
              "txSubmitPost: failed to submit transaction: "
                <> renderTxCmdError err
            liftIO $ Gauge.inc (tsmFailCount metrics)
            errorResponse (TxSubmitFail err)
          Right txid -> do
            liftIO $ logInfo trace $
              "txSubmitPost: successfully submitted transaction "
                <> renderMediumTxId txid
            liftIO $ Gauge.inc (tsmCount metrics)
            pure txid

-- | Render the first 16 characters of a transaction ID.
renderMediumTxId :: TxId -> Text
renderMediumTxId (TxId hash) = renderMediumHash hash

-- | Render the first 16 characters of a hex-encoded hash.
renderMediumHash :: Crypto.Hash crypto a -> Text
renderMediumHash = T.take 16 . T.decodeLatin1 . Crypto.hashToBytesAsHex
