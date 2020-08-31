-- |
-- Module:      GenBashrc.FilePath
-- Description: FilePath utilities.
-- Copyright:   (c) 2017-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensible; POSIX.
--
-- FilePath utilities.
module GenBashrc.FilePath
    ( haveExecutable
    , findExecutable
    , findExecutableAndFollowLinks
    , readlinkRecursive
    , dirInPath
    , checkFiles
    , checkFilesM
    , checkDirs
    , isInDir

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
import Control.Monad ((>=>), (>>=))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bool (Bool)
import Data.Foldable (any)
import Data.Function (($), (.), flip, on)
import Data.Functor ((<$>))
import qualified Data.List as List (isPrefixOf)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, isJust, maybe)
import Data.String (String)
import System.Environment (lookupEnv)
import System.IO (FilePath)

import System.Directory
    ( XdgDirectory
        ( XdgCache
        , XdgConfig
        , XdgData
        )
    , doesDirectoryExist
    , doesFileExist
    , doesPathExist
    , getHomeDirectory
    , getSymbolicLinkTarget
    , getXdgDirectory
    , pathIsSymbolicLink
    )
import qualified System.Directory as Directory (findExecutable)
import System.FilePath
    ( (</>)
    , dropTrailingPathSeparator
    , equalFilePath
    , getSearchPath
    , normalise
    , splitDirectories
    )


findExecutable :: MonadIO io => String -> io (Maybe FilePath)
findExecutable = liftIO . Directory.findExecutable

haveExecutable :: MonadIO io => String -> io Bool
haveExecutable exe = isJust <$> findExecutable exe

-- | Calls 'findExecutable' followed by 'readlinkRecursive'.  'Nothing' is
-- returned either if executable doesn't exist or if target of a symbolic link
-- doesn't exist.
findExecutableAndFollowLinks :: MonadIO io => String -> io (Maybe FilePath)
findExecutableAndFollowLinks =
    findExecutable >=> maybe (pure Nothing) readlinkRecursive

-- | If path is a symbolic link then read it's target.  Do this recursively
-- until target that is not a symbolic link is reached.  If target of a
-- symbolic link doesn't exist then 'Nothing' is returned.
readlinkRecursive :: MonadIO io => FilePath -> io (Maybe FilePath)
readlinkRecursive path = liftIO do
    pathExists <- doesPathExist path
    if pathExists
        then do
            isSymlink <- pathIsSymbolicLink path
            if isSymlink
                then
                    getSymbolicLinkTarget path >>= readlinkRecursive
                else
                    pure (Just path)
        else
            pure Nothing

dirInPath :: MonadIO io => FilePath -> io Bool
dirInPath dir = any (equalDirs dir) <$> liftIO getSearchPath
  where
    equalDirs :: FilePath -> FilePath -> Bool
    equalDirs = equalFilePath `on` (dropTrailingPathSeparator . normalise)

data UserDirectory
    = Home
    | Xdg XdgDirectory
    | DotLocal
    | XdgRuntime

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
userDir base relDir = liftIO case base of
    Home -> homeDir relDir
    Xdg d -> getXdgDirectory d relDir
    DotLocal -> homeDir (".local" </> relDir)
    XdgRuntime -> fromMaybe "/tmp" <$> lookupEnv "XDG_RUNTIME_DIR"
  where
    homeDir p = (</> p) <$> getHomeDirectory

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
userDirWithExistence base relDir = liftIO do
    dir <- userDir base relDir
    haveDir <- doesDirectoryExist dir
    pure . (, dir) $ if haveDir
        then Just dir
        else Nothing

-- | Find first file that exists, or return 'Nothing' of none.
checkFiles :: MonadIO io => [FilePath] -> io (Maybe FilePath)
checkFiles []             = pure Nothing
checkFiles (file : files) = liftIO do
    doesExist <- doesFileExist file
    if doesExist
        then pure (Just file)
        else checkFiles files

checkFilesM :: MonadIO io => [io FilePath] -> io (Maybe FilePath)
checkFilesM [] = pure Nothing
checkFilesM (getFile : getFiles) = do
    file <- getFile
    doesExist <- liftIO $ doesFileExist file
    if doesExist
        then pure (Just file)
        else checkFilesM getFiles

-- | Find first directory that exists, or return 'Nothing' of none.
checkDirs :: MonadIO io => [FilePath] -> io (Maybe FilePath)
checkDirs []           = pure Nothing
checkDirs (dir : dirs) = liftIO do
    doesExist <- doesDirectoryExist dir
    if doesExist
        then pure (Just dir)
        else checkDirs dirs

-- | Check if @path@ is in @dir@, i.e. @dir@ is a directory prefix of @path@.
--
-- @
-- \"\/foo\/bar\/baz" \`isInDir\` \"\/foo\"
-- @
isInDir :: FilePath -> FilePath -> Bool
isInDir = flip List.isPrefixOf `on` splitDirectories
