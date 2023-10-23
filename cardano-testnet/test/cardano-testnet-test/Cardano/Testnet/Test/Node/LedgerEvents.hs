{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Testnet.Test.Node.LedgerEvents
  ( hprop_ledger_events
  ) where

import           Cardano.Api

import           Cardano.Testnet

import           Prelude

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Except.Extra
import qualified Data.Text as Text
import           GHC.IO.Exception (IOException)
import           GHC.Stack (callStack)
import           System.FilePath ((</>))

import           Hedgehog
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Test.Base as H

import qualified Testnet.Property.Utils as H
import           Testnet.Runtime

newtype AdditionalCatcher
  = IOE IOException
  deriving Show


hprop_ledger_events :: Property
hprop_ledger_events = H.integrationRetryWorkspace 2 "shutdown-on-sigint" $ \tempAbsBasePath' -> do
  -- Start a local test net
  conf <- H.noteShowM $  mkConf tempAbsBasePath'

  let fastTestnetOptions = cardanoDefaultTestnetOptions
        { cardanoEpochLength = 50
        , cardanoSlotLength = 0.1
        }

  !testnetRuntime
    <- cardanoTestnet fastTestnetOptions conf
  NodeRuntime{nodeSprocket} <- H.headM $ poolRuntime <$> poolNodes testnetRuntime
  let socketName' = IO.sprocketName nodeSprocket
      socketBase = IO.sprocketBase nodeSprocket -- /tmp
      socketPath = socketBase </> socketName'

  H.note_ $ "Sprocket: " <> show nodeSprocket
  H.note_ $ "Abs path: " <> tempAbsBasePath'
  H.note_ $ "Socketpath: " <> socketPath


  !ret <- runExceptT $ handleIOExceptT IOE
                   $ runExceptT $ foldBlocks
                       (File $ configurationFile testnetRuntime)
                       (File socketPath)
                       FullValidation
                       [] -- Initial accumulator state
                       foldBlocksAccumulator
  case ret of
    Left (IOE e) ->
      H.failMessage callStack $ "foldBlocks failed with: " <> show e
    Right (Left e) ->
      H.failMessage callStack $ "foldBlocks failed with: " <> Text.unpack (renderFoldBlocksError e)
    Right (Right _v) -> success


foldBlocksAccumulator
  :: Env
  -> LedgerState
  -> [LedgerEvent]
  -> BlockInMode CardanoMode -- Block i
  -> [LedgerEvent] -- ^ Accumulator at block i - 1
  -> IO ([LedgerEvent], FoldStatus) -- ^ Accumulator at block i and fold status
foldBlocksAccumulator _ _ currentEvents _ acc =
  if any filterPoolReap currentEvents
  -- TODO: When we switch to a TChan in foldBlocks we won't have to keep track of
  -- the events ourselves.
  then return (currentEvents ++ acc, StopFold)
  else return (currentEvents ++ acc, ContinueFold)
 where
  -- We end the fold on PoolReap ledger event
  filterPoolReap :: LedgerEvent -> Bool
  filterPoolReap (PoolReap _) = True
  filterPoolReap _ = False


