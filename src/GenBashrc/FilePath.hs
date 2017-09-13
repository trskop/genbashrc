{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      GenBashrc.FilePath
-- Description: FilePath utilities.
-- Copyright:   (c) 2017 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensible; POSIX.
--
-- FilePath utilities.
module GenBashrc.FilePath
    ( haveExecutable
    , dirInPath
    )
  where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bool (Bool(False, True))
import Data.Foldable (any)
import Data.Function ((.), const, on)
import Data.Functor ((<$>))
import Data.Maybe (maybe)
import System.IO (FilePath)

import System.Directory (findExecutable)
import System.FilePath
    ( dropTrailingPathSeparator
    , equalFilePath
    , getSearchPath
    , normalise
    )


haveExecutable :: MonadIO io => FilePath -> io Bool
haveExecutable exe = maybe False (const True) <$> liftIO (findExecutable exe)

dirInPath :: MonadIO io => FilePath -> io Bool
dirInPath dir = any (equalDirs dir) <$> liftIO getSearchPath
  where
    equalDirs :: FilePath -> FilePath -> Bool
    equalDirs = equalFilePath `on` (dropTrailingPathSeparator . normalise)
