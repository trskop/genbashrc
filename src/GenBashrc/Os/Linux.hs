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
module GenBashrc.Os.Linux
--  (
--  )
  where

import Prelude (Bounded, Enum)

import Control.Applicative (Applicative)
import Control.Monad (guard)
import Data.Bool (Bool, otherwise)
import qualified Data.Char as Char (toLower)
import Data.Eq (Eq, (==))
import Data.Foldable (for_)
import Data.Function (($), (.), const)
import Data.Functor ((<$))
import qualified Data.Function as Function (id)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Typeable (cast)
import Text.Show (Show, show)

import Data.Text (Text)
import qualified Data.Text as Text (pack)

import GenBashrc.PackageManager
    ( HasPackageManager(getPackageManager)
    , IsPackageManager
    , SomePackageManager(SomePackageManager)
    , unknownPackageManager
    )


newtype LinuxInfo = LinuxInfo
    { distribution :: Distribution
    }
  deriving (Eq, Show)

instance HasPackageManager LinuxInfo where
    getPackageManager = getPackageManager . distribution

data PackageManager
    = Apt
    | Nix
    | Pacman
    | Yum
    -- ^ Yum or DNF.
  deriving (Bounded, Enum, Eq, Show)

instance IsPackageManager PackageManager

instance HasPackageManager PackageManager where
    getPackageManager = SomePackageManager

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

instance HasPackageManager DistributionId where
    getPackageManager =
        maybe unknownPackageManager SomePackageManager
        . distrubutionPackageManager

data Distribution = Distribution
    { id :: DistributionId
    , release :: Text
    , codeName :: Text
    }
  deriving (Eq, Show)

instance HasPackageManager Distribution where
    getPackageManager = getPackageManager . id

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

debianDistro :: DebianCodename -> Text -> Distribution
debianDistro cn rel = Distribution
    { id = Debian
    , release = rel
    , codeName = Text.pack . firstToLower $ show cn
    }
  where
    firstToLower ""       = ""
    firstToLower (c : cs) = Char.toLower c : cs

class HasDistributionId a where
    distributionId :: a -> DistributionId

    distrubutionPackageManager :: a -> Maybe PackageManager
    distrubutionPackageManager = distrubutionPackageManager . distributionId

instance HasDistributionId DistributionId where
    distributionId = Function.id

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

instance HasDistributionId Distribution where
    distributionId = id

instance HasDistributionId LinuxInfo where
    distributionId = distributionId . distribution

matchPackageManager
    :: (PackageManager -> Bool)
    -> SomePackageManager
    -> Maybe PackageManager
matchPackageManager p (SomePackageManager somePkgMgr) = do
    pkgMgr <- cast somePkgMgr
    pkgMgr <$ guard (p pkgMgr)

apt :: SomePackageManager -> Maybe PackageManager
apt = matchPackageManager (== Apt)

debian :: DistributionId -> Maybe DistributionId
debian = \case
    d@Debian -> Just d
    _ -> Nothing

debianCompat :: DistributionId -> Maybe DistributionId
debianCompat d = case d of
    Debian -> Just d
    Ubuntu -> Just d
    _ -> Nothing

neg :: (a -> Maybe b) -> a -> Maybe a
neg patternMatch x
  | Just _ <- patternMatch x = Nothing
  | otherwise = Just x

notDebianCompat :: DistributionId -> Maybe DistributionId
notDebianCompat = neg debianCompat

whenDistro
    :: (HasDistributionId a, Applicative f)
    => (DistributionId -> Maybe DistributionId)
    -> a
    -> (DistributionId -> f ())
    -> f ()
whenDistro patternMatch = for_ . patternMatch . distributionId

whenDistro_
    :: (HasDistributionId a, Applicative f)
    => (DistributionId -> Maybe DistributionId)
    -> a
    -> f ()
    -> f ()
whenDistro_ patternMatch distroId = whenDistro patternMatch distroId . const
