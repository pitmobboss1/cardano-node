{-# LANGUAGE DuplicateRecordFields #-}

module Testnet.Conf
  ( NodeConfigYamlFile(..)
  , Conf(..)
  , mkConf
  ) where

import           Testnet.Runtime

import qualified Hedgehog.Extras.Test.Base as H

newtype NodeConfigYamlFile = NodeConfigYamlFile
  { unNodeConfigYamlFile :: FilePath
  } deriving (Eq, Show)

newtype Conf = Conf
  { tempAbsPath :: TmpAbsolutePath
  } deriving (Eq, Show)

mkConf :: FilePath -> H.Integration Conf
mkConf tempAbsPath' =
  return $ Conf
    { tempAbsPath = TmpAbsolutePath tempAbsPath'
    }


