{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
-- |
-- Module:      GenBashrc.Os
-- Description: OS specific information.
-- Copyright:   (c) 2017 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- OS specific information.
module GenBashrc.Os
    ( OsInfo(..)
    , linux
    , macOs
    , whenOs
    , whenOs_
    , isOs
    )
  where

import Control.Applicative (Applicative)
import Data.Bool (Bool)
import Data.Eq (Eq)
import Data.Foldable (for_)
import Data.Function (($), (.), const)
import Data.Maybe (Maybe(Just, Nothing), isJust)
import Text.Show (Show)

import GenBashrc.Os.Linux (LinuxInfo)
import GenBashrc.Os.MacOs (MacOsInfo)
import GenBashrc.PackageManager (HasPackageManager(getPackageManager))


data OsInfo
    = Linux LinuxInfo
    | MacOs MacOsInfo
  deriving (Eq, Show)

instance HasPackageManager OsInfo where
    getPackageManager = \case
        Linux info -> getPackageManager info
        MacOs info -> getPackageManager info

-- TODO: Consider using Prisms for OS matching.

-- | Match when OS is Linux. See 'whenOs' for usage examples.
linux :: OsInfo -> Maybe LinuxInfo
linux = \case
    Linux info -> Just info
    _ -> Nothing

-- | Match when OS is MacOs. See 'whenOs' for usage examples.
macOs :: OsInfo -> Maybe MacOsInfo
macOs = \case
    MacOs info -> Just info
    _ -> Nothing

-- | Perform action only on specific OS.
--
-- @
-- ... = do
--     'whenOs' 'linux' '$' \\linuxInfo -> do
--         ...
-- @
whenOs :: Applicative f => (OsInfo -> Maybe a) -> OsInfo -> (a -> f ()) -> f ()
whenOs patternMatch = for_ . patternMatch

-- | Perform action only on specific OS. Same as 'whenOs', but action doesn't
-- get access to OS specific information.
--
-- @
-- ... = do
--     'whenOs_' 'linux' '$' do
--         ...
-- @
whenOs_ :: Applicative f => (OsInfo -> Maybe a) -> OsInfo -> f () -> f ()
whenOs_ patternMatch os action = whenOs patternMatch os (const action)

isOs :: (OsInfo -> Maybe a) -> OsInfo -> Bool
isOs = (isJust .) . ($)
