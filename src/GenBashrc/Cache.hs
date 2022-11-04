-- |
-- Module:      GenBashrc.Cache
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2019-2022 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- TODO: Module description.
module GenBashrc.Cache
    ( Cachable(..)
    , cache
    , cached
    , getCacheStdFilePath
    )
  where

import Control.Applicative (pure)
import Control.Monad (unless)
import Data.Bool (Bool(True))
import Data.Functor ((<$))
import Data.Monoid ((<>))
import Data.String (String)
import System.IO (FilePath, IO)

import Data.Text (Text)
import qualified Data.Text.IO as Text (readFile, writeFile)
import qualified Data.Text.Lazy as Lazy (Text)
import qualified Data.Text.Lazy.IO as Lazy.Text (readFile, writeFile)
import Data.String.ToString (ToString, toString)
import System.Directory
    ( XdgDirectory(XdgCache)
    , createDirectoryIfMissing
    , doesFileExist
    , getXdgDirectory
    )
import System.FilePath ((</>), takeDirectory)


class Cachable a where
    writeCache :: FilePath -> a -> IO ()
    readCache :: FilePath -> IO a

instance Cachable Text where
    writeCache :: FilePath -> Text -> IO ()
    writeCache = Text.writeFile

    readCache :: FilePath -> IO Text
    readCache = Text.readFile

instance Cachable Lazy.Text where
    writeCache :: FilePath -> Lazy.Text -> IO ()
    writeCache = Lazy.Text.writeFile

    readCache :: FilePath -> IO Lazy.Text
    readCache file = do
        b <- Lazy.Text.readFile file
        pure ("# Cached\n" <> b)

cache :: Cachable a => FilePath -> a -> IO ()
cache file content = do
    cacheExists <- doesFileExist file
    unless cacheExists do
        createDirectoryIfMissing True (takeDirectory file)
        writeCache file content

cached :: Cachable b => FilePath -> (a -> b) -> a -> IO b
cached file f a = do
    let b = f a
    b <$ cache file b

-- | Generate a standard cache file path:
--
-- > ${XDG_CACHE_HOME:-${HOME}/.cache}/${appName}/${fileName}
getCacheStdFilePath
    :: ToString fileName
    => String
    -- ^ Application name.
    -> fileName
    -- ^ File name, probably a hash.
    -> IO FilePath
getCacheStdFilePath appName fileName =
    getXdgDirectory XdgCache (appName </> toString fileName)
