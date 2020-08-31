-- |
-- Module:      GenBashrc.SystemInfo
-- Description: Utilities for gathering system (HW, OS, etc.) information.
-- Copyright:   (c) 2017 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensible; POSIX.
--
-- Utilities for gathering system (HW, OS, etc.) information.
module GenBashrc.SystemInfo
    ( haveCdrom
    , detectOs
    )
  where

import Prelude (error)

import Control.Applicative (pure)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bool (Bool)
import System.Info (os)

import System.Directory (doesFileExist)

import GenBashrc.Os
import GenBashrc.Os.MacOs (MacOsInfo(MacOsInfo))
import GenBashrc.Os.Linux (LinuxInfo(LinuxInfo, distribution))
import qualified GenBashrc.Os.Linux as Linux
    ( DebianCodename(Buster)
    , debianDistro
    )


haveCdrom :: MonadIO io => io Bool
haveCdrom = liftIO (doesFileExist "/proc/sys/dev/cdrom")

detectOs :: MonadIO io => io OsInfo
detectOs = pure case os of
    "darwin" -> MacOs MacOsInfo -- TODO
    "linux" -> Linux LinuxInfo
        { distribution = Linux.debianDistro Linux.Buster "testing"  -- TODO
        }
    _ -> error "Unknown OS" -- TODO
