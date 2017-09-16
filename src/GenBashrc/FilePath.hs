{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
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
    , checkFiles
    , checkFilesM
    , checkDirs

    -- * User Directories
    , UserDirectory(..)
    , xdgCache
    , xdgConfig
    , xdgData
    , userDir
    , userDirWithExistence
    , (<</>)
    , (?<</>)
    )
  where

import Control.Applicative (pure)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bool (Bool)
import Data.Foldable (any)
import Data.Function (($), (.), on)
import Data.Functor ((<$>))
import Data.Maybe (Maybe(Just, Nothing), isJust)
import System.IO (FilePath)

import System.Directory
    ( XdgDirectory
        ( XdgCache
        , XdgConfig
        , XdgData
        )
    , doesDirectoryExist
    , doesFileExist
    , findExecutable
    , getHomeDirectory
    , getXdgDirectory
    )
import System.FilePath
    ( (</>)
    , dropTrailingPathSeparator
    , equalFilePath
    , getSearchPath
    , normalise
    )


haveExecutable :: MonadIO io => FilePath -> io Bool
haveExecutable exe = isJust <$> liftIO (findExecutable exe)

dirInPath :: MonadIO io => FilePath -> io Bool
dirInPath dir = any (equalDirs dir) <$> liftIO getSearchPath
  where
    equalDirs :: FilePath -> FilePath -> Bool
    equalDirs = equalFilePath `on` (dropTrailingPathSeparator . normalise)

data UserDirectory
    = Home
    | Xdg XdgDirectory
    | DotLocal

xdgConfig :: UserDirectory
xdgConfig = Xdg XdgConfig

xdgData :: UserDirectory
xdgData = Xdg XdgData

xdgCache :: UserDirectory
xdgCache = Xdg XdgCache

-- | Infix alias for 'userDir'.
(<</>)
    :: MonadIO io
    => UserDirectory
    -> FilePath
    -- ^ Directory\/file FilePath relative to 'UserDirectory'.
    -> io FilePath
(<</>) = userDir
infixr 5 <</>

userDir
    :: MonadIO io
    => UserDirectory
    -> FilePath
    -- ^ Directory\/file FilePath relative to 'UserDirectory'.
    -> io FilePath
userDir base relDir = liftIO $ case base of
    Home -> homeDir relDir
    Xdg d -> liftIO $ getXdgDirectory d relDir
    DotLocal -> homeDir (".local" </> relDir)
  where
    homeDir p = (</> p) <$> liftIO getHomeDirectory

-- | Infix alias for 'userDirWithExistence'.
(?<</>)
    :: MonadIO io
    => UserDirectory
    -> FilePath
    -- ^ Directory\/file FilePath relative to 'UserDirectory'.
    -> io (Maybe FilePath, FilePath)
(?<</>) = userDirWithExistence
infixr 5 ?<</>

userDirWithExistence
    :: MonadIO io
    => UserDirectory
    -> FilePath
    -- ^ Directory\/file FilePath relative to 'UserDirectory'.
    -> io (Maybe FilePath, FilePath)
userDirWithExistence base relDir = do
    dir <- userDir base relDir
    haveDir <- liftIO $ doesDirectoryExist dir
    pure . (, dir) $ if haveDir
        then Just dir
        else Nothing

-- | Find first file that exists, or return 'Nothing' of none.
checkFiles :: MonadIO io => [FilePath] -> io (Maybe FilePath)
checkFiles []             = pure Nothing
checkFiles (file : files) = do
    doesExist <- liftIO $ doesFileExist file
    if doesExist
        then pure $ Just file
        else checkFiles files

checkFilesM :: MonadIO io => [io FilePath] -> io (Maybe FilePath)
checkFilesM [] = pure Nothing
checkFilesM (getFile : getFiles) = do
    file <- getFile
    doesExist <- liftIO $ doesFileExist file
    if doesExist
        then pure $ Just file
        else checkFilesM getFiles

-- | Find first directory that exists, or return 'Nothing' of none.
checkDirs :: MonadIO io => [FilePath] -> io (Maybe FilePath)
checkDirs []           = pure Nothing
checkDirs (dir : dirs) = do
    doesExist <- liftIO $ doesDirectoryExist dir
    if doesExist
        then pure $ Just dir
        else checkDirs dirs
