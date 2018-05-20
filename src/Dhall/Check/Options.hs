{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Dhall.Check.Options where

import Data.Text (Text)
import Options.Generic
import System.Directory (getCurrentDirectory)

data Options = Options
  { fileExtension :: String
  , directory     :: Maybe FilePath
  } deriving (Generic, Show)

instance ParseRecord Options

getOptions :: Text -> IO Options
getOptions help = getRecord help

defaultDirectory :: IO FilePath
defaultDirectory = getCurrentDirectory
