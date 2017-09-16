{-# LANGUAGE NoImplicitPrelude #-}
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
    )
  where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bool (Bool)
import Data.Function (($))

import System.Directory (doesFileExist)


haveCdrom :: MonadIO io => io Bool
haveCdrom = liftIO $ doesFileExist "/proc/sys/dev/cdrom"
