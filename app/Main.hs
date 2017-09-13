{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
module Main (main)
  where

import Prelude (error)

import Control.Applicative (Applicative, (*>), (<*>), liftA2, pure)
import Control.Monad ((>>=), guard, unless, when)
import Data.Bool (Bool(False), (&&), (||), not)
import Data.Eq (Eq)
import Data.Foldable (asum)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(Just, Nothing), isJust, maybe)
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import System.Environment (getArgs)
import System.IO (FilePath, IO)
import Text.Show (Show)

import Data.Text (Text)
import qualified Data.Text.Lazy as Lazy (Text)
import qualified Data.Text.Lazy.IO as Lazy.Text (putStr, writeFile)
--import qualified Data.Text as Text (pack)
import Network.HostName (HostName, getHostName)
import System.Directory (doesDirectoryExist, doesFileExist, getHomeDirectory)
import System.FilePath ((</>))

import GenBashrc.Bash
import GenBashrc.Distribution
import GenBashrc.FilePath

--import Debug.Trace (traceShowId)


data Context = Context
    { hostname :: HostName
    , packageManger :: Maybe PackageManager
    , distribution :: Distribution
    , haveTouchpad :: Bool
    , haveSudo :: Bool
    , haveMplayer :: Bool
    , haveXpdfCompat :: Bool
    , haveColorDiff :: Bool
    , haveScreen :: Bool
    , haveNeovim :: Bool
    , haveVim :: Bool
    , haveLessPipe :: Bool
    , haveDircolors :: Bool
    , haveXinput :: Bool
    , haveCdrom :: Bool
    , haveGit :: Bool
    , canCloseCdrom :: Bool
    , home :: FilePath
    , userBinDir :: Maybe FilePath
    , userBinDir' :: FilePath
    , userLocalBinDir :: Maybe FilePath
    , userLocalBinDir' :: FilePath
    , bashCompletionScript :: Maybe FilePath
    , gitPromptScript :: Maybe FilePath
    , userDircolors :: Maybe FilePath
    }
  deriving (Eq, Show)

context :: IO Context
context = do
    homeDir <- getHomeDirectory
    mkContext
        <$> getHostName
        <*> pure homeDir
        <*> getUserDir (homeDir </> "bin")
        <*> getUserDir (homeDir </> ".local" </> "bin")
        <*> haveExecutable "sudo"
        <*> haveExecutable "vim"
        <*> haveExecutable "nvim"
        <*> haveExecutable "mplayer"
        <*> ( haveExecutable "xpdf-compat"
            `orA` doesFileExist (homeDir </> "bin" </> "xpdf-compat")
            )
        <*> haveExecutable "colordiff"
        <*> haveExecutable "screen"
        <*> haveExecutable "lesspipe"
        <*> haveExecutable "dircolors"
        <*> haveExecutable "xinput"
        <*> haveExecutable "git"
        <*> lookupBashCompletionScript
        <*> lookupGitPromptScript
        <*> checkFiles [homeDir </> ".dircolors"]
  where
    mkContext hn homeDir (binDir, binDir') (localBinDir, localBinDir') sudo vim
      neovim mplayer xpdfCompat colordiff screen lesspipe' dircolors xinput git
      bashCompletionScript' gitPromptScript' userDircolors' =
        Context
            { hostname = hn
            , packageManger = distrubutionPackageManager Debian -- TODO
            , distribution = debian Buster "testing" -- TODO
            , haveTouchpad = False -- TODO
                -- $ grep "^N: Name=.* Touchpad" /proc/bus/input/devices
                -- N: Name="ELAN1200:00 04F3:3059 Touchpad"
            , haveSudo = sudo
            , haveMplayer = mplayer
            , haveXpdfCompat = xpdfCompat
            , haveColorDiff = colordiff
            , haveScreen = screen
            , haveNeovim = neovim
            , haveVim = vim
            , haveLessPipe = lesspipe'
            , haveDircolors = dircolors
            , haveXinput = xinput
            , haveCdrom = False -- TODO
            , haveGit = git
            , canCloseCdrom = False -- TODO
            , home = homeDir
            , userBinDir = binDir
            , userBinDir' = binDir'
            , userLocalBinDir = localBinDir
            , userLocalBinDir' = localBinDir'
            , bashCompletionScript = bashCompletionScript'
            , gitPromptScript = gitPromptScript'
            , userDircolors = userDircolors'
            }

    getUserDir dir = do
        haveDir <- doesDirectoryExist dir
        pure . (, dir) $ if haveDir
            then Just dir
            else Nothing

    orA = liftA2 (||)

    lookupBashCompletionScript = checkFiles
        [ "/usr/share/bash-completion/bash_completion"
        , "/etc/bash_completion"
        ]

    lookupGitPromptScript = checkFiles
        [ "/usr/lib/git-core/git-sh-prompt"
        , "/etc/bash_completion.d/git-prompt"
        ]

    checkFiles :: [FilePath] -> IO (Maybe FilePath)
    checkFiles []             = pure Nothing
    checkFiles (file : files) = do
        doesExist <- doesFileExist file
        if doesExist
            then pure $ Just file
            else checkFiles files

aliases :: Context -> Bash ()
aliases Context{..} = do
    withDircollorsWhen haveDircolors userDircolors $ do
        alias "ls" "'ls --color=auto'"
        alias "dir" "'dir --color=auto'"
        alias "vdir" "'vdir --color=auto'"

        alias "egrep" "'egrep --color=auto'"
        alias "fgrep" "'fgrep --color=auto'"
        alias "grep" "'grep --color=auto'"

        when haveColorDiff $ alias "diff" "colordiff"

    alias "cp" "'cp -i'"
    alias "mv" "'mv -i'"
    alias "rm" "'rm -i'"

    whenIsApt packageManger $ do
        alias "apt-get" "'sudo apt-get'"
        alias "apt"     "'sudo apt'"

    alias "evil" $ if haveSudo then "'sudo su -'" else "'su -'"

    when haveMplayer $ alias "mplayer" "'mplayer -idx'"
    when haveXpdfCompat $ alias "xpdf" "xpdf-compat"
    alias "term-title" "'printf \"\\033]2;%s\\007\"'"

    when (haveTouchpad && haveXinput) $ do
        -- TODO: Rewrite following to use xinput.
        alias "touchpad-off" "'synclient TouchpadOff=1'"
        alias "touchpad-on"  "'synclient TouchpadOff=0'"

    when haveCdrom $ do
        alias "eject" "eject -d"
        unless canCloseCdrom $ alias "close" "eject -d -t"

history :: Context -> Bash ()
history _ = do
    -- Append to the history file, don't overwrite it.
    shopt Set Histappend

    -- Don't put duplicate lines in the history.
    histcontrol (Just Append) [HistIgnoredups]

    -- Increase history size to some very high, but sensible value. It's not as
    -- fast on Windows as you might expect coming from UNIX/Linux.
    histsize $ Just 100000

    -- Don't truncate history file.
    histfilesize Nothing

setPrompt :: Context -> Bash ()
setPrompt Context{..} = do
    when haveScreen
        . function "__screen_ps1" $ do
            line @Text "[[ -n \"${WINDOW}\" ]] && echo \"#${WINDOW}\""

    prompt (Proxy @'PS1) $ "'\\[\\e[32m\\][ \\[\\e[37m\\]\\u@\\h" <> screenPs1
        <> " \\W" <> gitPs1 <> "\\[\\e[32m\\] ]\\$\\[\\e[22;0m\\] '"
    exportPrompt (Proxy @'PS1)
  where
    screenPs1 = if haveScreen then "$(__screen_ps1)" else ""
    gitPs1 = if haveGit && isJust gitPromptScript then "$(__git_ps1)" else ""

editor :: Context -> Bash ()
editor Context{..} = asum
    [ do
        guard haveNeovim
        setEditor "nvim"
    , do
        guard haveVim
        setEditor "vim"
    ]

-- Ideas:
--
-- * Run this application as a service. Output must be made available to Bash
--   atomically.
-- * Track dependencies and only when something changes produce new output.

main :: IO ()
main = getArgs >>= \case
    [] -> main' Lazy.Text.putStr
    [outputFileName] -> do
        main' $ Lazy.Text.writeFile outputFileName
    _ -> error "Too many arguments."

main' :: (Lazy.Text -> IO a) -> IO a
main' writeOutput = do
    ctx@Context{..} <- context
    userBinDirInPath <- dirInPath userBinDir'
    userLocalBinDirInPath <- dirInPath userLocalBinDir'
    writeOutput . genBash $ do
        -- Report job state changes immediately and don't wait for printing new
        -- prompt.
        shopt Set NotifyOfJobTerminationImmediately

        -- Turn on vi-style line editing interface.
        shopt Set Vi

        -- Use visible bell (if available) instead of audible, which is the
        -- default.
        line @Text "set bell-style visible"

        -- Check the window size after each command and, if necessary, update
        -- the values of LINES and COLUMNS.
        shopt Set Checkwinsize

        history ctx

        lesspipeWhen haveLessPipe

        pathUpdated <- updatePath Prepend $ PathElements
            [ guard (not userBinDirInPath) *> userBinDir
            , guard (not userLocalBinDirInPath) *> userLocalBinDir
            ]
        exportPathIf pathUpdated

        setPrompt ctx

        editor ctx

        aliases ctx

        onJust bashCompletionScript $ \script ->
            bashIfThen "! shopt -oq posix" $ source script

        when haveGit $ onJust gitPromptScript source

onJust :: Applicative f => Maybe a -> (a -> f ()) -> f ()
onJust x f = maybe (pure ()) f x
