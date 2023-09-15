{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}


module Testnet.Components.Configuration
  ( createConfigYaml
  ) where

import           Cardano.Api.Shelley hiding (cardanoEra)

import           Control.Monad.IO.Class (MonadIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import           GHC.Stack (HasCallStack)
import qualified GHC.Stack as GHC
import           System.FilePath.Posix ((</>))

import           Hedgehog

import           Testnet.Defaults
import           Testnet.Property.Utils
import           Testnet.Runtime

createConfigYaml
  :: (MonadTest m, MonadIO m, HasCallStack)
  => TmpAbsolutePath
  -> AnyCardanoEra
  -> m LBS.ByteString
createConfigYaml tempAbsPath anyCardanoEra' = GHC.withFrozenCallStack $ do
  let tempAbsPath' = unTmpAbsPath tempAbsPath

  -- Add Byron, Shelley and Alonzo genesis hashes to node configuration
  -- TODO: These genesis filepaths should not be hardcoded. Using the cli as a library
  -- rather as an executable will allow us to get the genesis files paths in a more
  -- direct fashion.

  byronGenesisHash <- getByronGenesisHash $ tempAbsPath' </> "byron/genesis.json"
  shelleyGenesisHash <- getShelleyGenesisHash (tempAbsPath' </> "shelley/genesis.json") "ShelleyGenesisHash"
  alonzoGenesisHash <- getShelleyGenesisHash (tempAbsPath' </> "shelley/genesis.alonzo.json") "AlonzoGenesisHash"
  conwayGenesisHash <- getShelleyGenesisHash (tempAbsPath' </> "shelley/genesis.conway.json") "ConwayGenesisHash"


  return . Aeson.encode . Aeson.Object
    $ mconcat [ byronGenesisHash
              , shelleyGenesisHash
              , alonzoGenesisHash
              , conwayGenesisHash
              , defaultYamlHardforkViaConfig anyCardanoEra'
              ]
