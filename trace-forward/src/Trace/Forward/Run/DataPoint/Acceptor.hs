{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}

module Trace.Forward.Run.DataPoint.Acceptor
  ( acceptDataPointsInit
  , acceptDataPointsResp
  ) where

import qualified Codec.Serialise as CBOR
import           Control.Concurrent.STM.TMVar (putTMVar)
import           Control.Concurrent.STM.TVar (modifyTVar', readTVar, readTVarIO)
import           Control.Exception (finally)
import           Control.Monad (unless)
import           Control.Monad.Extra (ifM)
import           Control.Monad.STM (atomically, check)
import qualified Data.ByteString.Lazy as LBS
import           Data.Void (Void)
import           Ouroboros.Network.Driver.Simple (runPeer)
import           Ouroboros.Network.Mux (MiniProtocolCb (..), MuxMode (..), RunMiniProtocol (..))

import           Trace.Forward.Configuration.DataPoint (AcceptorConfiguration (..))
import qualified Trace.Forward.Protocol.DataPoint.Acceptor as Acceptor
import qualified Trace.Forward.Protocol.DataPoint.Codec as Acceptor
import           Trace.Forward.Protocol.DataPoint.Type (DataPointName)
import           Trace.Forward.Utils.DataPoint (DataPointRequestor (..))

acceptDataPointsInit
  :: AcceptorConfiguration
  -> (initiatorCtx -> IO DataPointRequestor)
  -> (initiatorCtx -> IO ())
  -> RunMiniProtocol 'InitiatorMode initiatorCtx responderCtx LBS.ByteString IO () Void
acceptDataPointsInit config mkDPRequestor peerErrorHandler =
  InitiatorProtocolOnly $ runPeerWithRequestor config mkDPRequestor peerErrorHandler

acceptDataPointsResp
  :: AcceptorConfiguration
  -> (responderCtx -> IO DataPointRequestor)
  -> (responderCtx -> IO ())
  -> RunMiniProtocol 'ResponderMode initiatorCtx responderCtx LBS.ByteString IO Void ()
acceptDataPointsResp config mkDPRequestor peerErrorHandler =
  ResponderProtocolOnly $ runPeerWithRequestor config mkDPRequestor peerErrorHandler

runPeerWithRequestor
  :: AcceptorConfiguration
  -> (ctx -> IO DataPointRequestor)
  -> (ctx -> IO ())
  -> MiniProtocolCb ctx LBS.ByteString IO ()
runPeerWithRequestor config mkDPRequestor peerErrorHandler =
  MiniProtocolCb $ \ctx channel -> do
    dpRequestor <- mkDPRequestor ctx
    runPeer
      (acceptorTracer config)
      (Acceptor.codecDataPointForward CBOR.encode CBOR.decode
                                      CBOR.encode CBOR.decode)
      channel
      (Acceptor.dataPointAcceptorPeer $ acceptorActions config dpRequestor [])
    `finally` peerErrorHandler ctx

acceptorActions
  :: AcceptorConfiguration
  -> DataPointRequestor
  -> [DataPointName]
  -> Acceptor.DataPointAcceptor IO ()
acceptorActions config@AcceptorConfiguration{shouldWeStop}
                dpRequestor@DataPointRequestor{askDataPoints, dataPointsNames, dataPointsReply}
                dpNames =
  Acceptor.SendMsgDataPointsRequest dpNames $ \replyWithDataPoints -> do
    -- Ok, reply with 'DataPoint's is already here, update the requestor.
    unless (null replyWithDataPoints) $ atomically $ do
      -- Store the reply for acceptor's external context.
      putTMVar dataPointsReply replyWithDataPoints
      -- To prevent new automatic request.
      modifyTVar' askDataPoints $ const False
    ifM (readTVarIO shouldWeStop)
      (return $ Acceptor.SendMsgDone $ return ())
      $ do
        -- Block here until external context explicitly ask for 'DataPoint's again.
        atomically $ readTVar askDataPoints >>= check
        -- Ok, external context asked for 'DataPoint's, take their names.
        dpNames' <- readTVarIO dataPointsNames
        -- Ask.
        return $ acceptorActions config dpRequestor dpNames'
