-- |
-- Module:      GenBashrc.Os.MacOs
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2017 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO: Module description.
module GenBashrc.Os.MacOs
--  (
--  )
  where

import Data.Eq (Eq)
import Text.Show (Show)

import GenBashrc.Bash (Bash, setAndExport)
import GenBashrc.PackageManager
    ( HasPackageManager(getPackageManager)
    , IsPackageManager
    , SomePackageManager(SomePackageManager)
    )


data MacOsInfo = MacOsInfo  -- TODO
  deriving stock (Eq, Show)

instance HasPackageManager MacOsInfo where
    getPackageManager _info = SomePackageManager Homebrew -- TODO

data PackageManager
    = Homebrew
    | MacPorts
  deriving stock (Eq, Show)

instance IsPackageManager PackageManager

instance HasPackageManager PackageManager where
    getPackageManager = SomePackageManager

clicolor :: Bash ()
clicolor = setAndExport "CLICOLOR" "1"
