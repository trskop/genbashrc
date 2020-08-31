-- |
-- Module:      GenBashrc.Cache
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- TODO: Module description.
module GenBashrc.Cache
    ( Cachable(..)
    , cached
    , getCacheStdFilePath
    )
  where

import Control.Applicative (pure)
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

cached :: Cachable b => FilePath -> (a -> b) -> a -> IO b
cached file f a = do
    cacheExists <- doesFileExist file
    if cacheExists
        then readCache file
        else
            let b = f a
            in b <$ do
                createDirectoryIfMissing True (takeDirectory file)
                writeCache file b

getCacheStdFilePath
    :: ToString hash
    => String
    -> hash
    -> hash
    -> IO FilePath
getCacheStdFilePath appName exeHash contextHash =
    getXdgDirectory XdgCache (appName </> fileName)
  where
    fileName = toString exeHash <> "-" <> toString contextHash
