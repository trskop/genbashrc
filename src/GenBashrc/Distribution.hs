{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
-- |
-- Module:      GenBashrc.Distribution
-- Description: Linux distribution specifics.
-- Copyright:   (c) 2017 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensible; POSIX.
--
-- Linux distribution specifics.
module GenBashrc.Distribution
--  (
--  )
  where

import Prelude (Bounded, Enum)

import Control.Applicative (Applicative, pure)
import qualified Data.Char as Char (toLower)
import Data.Eq (Eq)
import Data.Function (($), (.))
import Data.Maybe (Maybe(Just, Nothing))
import Text.Show (Show, show)

import Data.Text (Text)
import qualified Data.Text as Text (pack)


data PackageManager
    = Apt
    | Nix
    | Pacman
    | Yum
    -- ^ Yum or DNF.
  deriving (Bounded, Enum, Eq, Show)

data DistributionId
    = Alpine
    | Arch
    | CentOs
    | Debian
    | Fedora
    | NixOs
    | Rhel
    -- ^ Red Hat Enterprise Linux
    | Ubuntu
    | OtherDistributionId Text
  deriving (Eq, Show)

data Distribution = Distribution
    { id :: DistributionId
    , release :: Text
    , codeName :: Text
    }
  deriving (Eq, Show)

data DebianCodename
    = Buzz
    | Rex
    | Bo
    | Hamm
    | Slink
    | Potato
    | Woody
    | Sarge
    | Etch
    | Lenny
    | Squeeze
    | Wheezy
    | Jessie
    | Stretch
    | Buster
    | Bullseye
  deriving (Bounded, Enum, Eq, Show)

debian :: DebianCodename -> Text -> Distribution
debian cn rel = Distribution
    { id = Debian
    , release = rel
    , codeName = Text.pack . firstToLower $ show cn
    }
  where
    firstToLower ""       = ""
    firstToLower (c : cs) = Char.toLower c : cs

distrubutionPackageManager :: DistributionId -> Maybe PackageManager
distrubutionPackageManager = \case
    Alpine -> Nothing
    Arch -> Just Pacman
    CentOs -> Just Yum
    Debian -> Just Apt
    Fedora -> Just Yum
    NixOs -> Just Nix
    Rhel -> Just Yum
    Ubuntu -> Just Apt
    OtherDistributionId _ -> Nothing

whenIsApt :: Applicative f => Maybe PackageManager -> f () -> f ()
whenIsApt (Just Apt) m = m
whenIsApt _          _ = pure ()

whenIsDebian :: Applicative f => DistributionId -> f () -> f ()
whenIsDebian Debian m = m
whenIsDebian _      _ = pure ()

whenIsDebianComapt :: Applicative f => DistributionId -> f () -> f ()
whenIsDebianComapt distroId m = case distroId of
    Debian -> m
    Ubuntu -> m
    _ -> pure ()
