{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
module Main (main)
  where

import Prelude (error)

import Control.Applicative (Applicative, (*>), liftA2, pure)
import Control.Monad ((>>=), guard, unless, when)
import Data.Bool (Bool(False), (&&), (||), not)
import Data.Eq (Eq)
import Data.Foldable (for_)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(Just, Nothing), isJust, maybe)
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Data.String (fromString)
import System.Environment (getArgs)
import System.IO (FilePath, IO)
import Text.Show (Show)

import Data.Text (Text)
import qualified Data.Text.Lazy as Lazy (Text)
import qualified Data.Text.Lazy.IO as Lazy.Text (putStr, writeFile)
import Network.HostName (HostName, getHostName)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

import GenBashrc.Bash
import GenBashrc.FilePath
import GenBashrc.Os
import qualified GenBashrc.Os.Linux as Linux
import GenBashrc.PackageManager
import qualified GenBashrc.SystemInfo as SystemInfo
import qualified GenBashrc.Utils as Utils

--import Debug.Trace (traceShowId)


data Context = Context
    { currentOs :: OsInfo
    , hostname :: HostName
    , haveTouchpad :: Bool
    , haveSudo :: Bool
    , haveMplayer :: Bool
    , haveXpdfCompat :: Bool
    , haveColorDiff :: Bool
    , haveScreen :: Bool
    , haveTmux :: Bool
    , haveNeovim :: Bool
    , haveVim :: Bool
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
    , lesspipeCommand :: Maybe CommandName
    , stackBin :: Maybe FilePath
    , tmuxConfig :: Maybe FilePath
    , fzfBashrc :: Maybe FilePath
    }
  deriving (Eq, Show)

context :: IO Context
context = do
    currentOs <- SystemInfo.detectOs
    hostname <- getHostName
    home <- getHomeDirectory
    (userBinDir, userBinDir') <- (Home ?<</> "bin")
    (userLocalBinDir, userLocalBinDir') <- (DotLocal ?<</> "bin")
    haveSudo <- haveExecutable "sudo"
    haveVim <- haveExecutable "vim"
    haveNeovim <- haveExecutable "nvim"
    haveMplayer <- haveExecutable "mplayer"
    haveXpdfCompat <- haveXpdfCompatExecutable
    haveColorDiff <- haveExecutable "colordiff"
    haveScreen <- haveExecutable "screen"
    haveTmux <- haveExecutable "tmux"
    haveDircolors <- haveExecutable "dircolors"
    haveXinput <- haveExecutable "xinput"
    haveGit <- haveExecutable "git"
    lesspipeCommand <- Utils.lookupLesspipeCommand
    bashCompletionScript <- Utils.lookupBashCompletionScript
    gitPromptScript <- Utils.lookupGitPromptScript currentOs
    userDircolors <- checkFilesM [Home <</> ".dircolors"]
    haveCdrom <- SystemInfo.haveCdrom
    stackBin <- Utils.lookupStack
    tmuxConfig <- checkFilesM [xdgConfig <</> "tmux" </> "tmux.conf"]
    fzfBashrc <- Utils.lookupFzfBashrc

    pure Context
        { hostname
        , currentOs
        , haveTouchpad = False -- TODO
            -- $ grep "^N: Name=.* Touchpad" /proc/bus/input/devices
            -- N: Name="ELAN1200:00 04F3:3059 Touchpad"
        , haveSudo
        , haveMplayer
        , haveXpdfCompat
        , haveColorDiff
        , haveScreen
        , haveTmux
        , haveNeovim
        , haveVim
        , haveDircolors
        , haveXinput
        , haveCdrom
        , haveGit
        , canCloseCdrom = False -- TODO
        , home
        , userBinDir
        , userBinDir'
        , userLocalBinDir
        , userLocalBinDir'
        , bashCompletionScript
        , gitPromptScript
        , userDircolors
        , lesspipeCommand
        , stackBin
        , tmuxConfig
        , fzfBashrc
        }
  where
    orA = liftA2 (||)

    haveXpdfCompatExecutable = haveExecutable "xpdf-compat"
        `orA`
            ( isJust <$> checkFilesM
                [ Home <</> "bin" </> "xpdf-compat"
                , DotLocal <</> "bin" </> "xpdf-compat"
                ]
            )

vimAliasForNeovim :: Context -> Bash ()
vimAliasForNeovim Context{haveNeovim} = when haveNeovim $ alias "vim" "nvim"
    -- TODO: Consider using full path to neovim binary.

aliases :: Context -> Bash ()
aliases ctx@Context{..} = do
    Utils.standardAliases Utils.AliasOptions{..}

    whenOs linux currentOs $ \linuxOs -> do
        Linux.whenDistro_ Linux.notDebianCompat linuxOs
            $ vimAliasForNeovim ctx

        whenPackageManager_ Linux.apt linuxOs $ do
            alias "apt-get" "'sudo apt-get'"
            alias "apt"     "'sudo apt'"

        when (haveTouchpad && haveXinput) $ do
            -- TODO: Rewrite following to use xinput.
            alias "touchpad-off" "'synclient TouchpadOff=1'"
            alias "touchpad-on"  "'synclient TouchpadOff=0'"

        when haveCdrom $ do
            alias "eject" "eject -d"
            unless canCloseCdrom $ alias "close" "eject -d -t"

    whenOs_ macOs currentOs
        $ vimAliasForNeovim ctx

    alias "evil" $ if haveSudo then "'sudo su -'" else "'su -'"

    when haveMplayer $ alias "mplayer" "'mplayer -idx'"
    when haveXpdfCompat $ alias "xpdf" "xpdf-compat"

    when haveTmux
        . alias "tmux"
            $ "'TERM=xterm-256color tmux"
            <> maybe "" (\cfg -> " -f \"" <> fromString cfg <> "\"") tmuxConfig
            <> "'"

    alias "term-title" "'printf \"\\033]2;%s\\007\"'"

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
        . function "__screen_ps1"
            $ line @Text "[[ -n \"${WINDOW}\" ]] && echo \"#${WINDOW}\""

    prompt (Proxy @'PS1)
        $ "'\\[\\e[32m\\][ \\[\\e[37m\\]\\u@\\h"
        <> screenPs1
        <> " \\W${CD_LEVEL:+⟨${CD_LEVEL}⟩}"
        <> gitPs1
        <> "\\[\\e[32m\\] ]\\$\\[\\e[22;0m\\] '"
    exportPrompt (Proxy @'PS1)
  where
    screenPs1 = if haveScreen then "$(__screen_ps1)" else ""
    gitPs1 = if haveGit && isJust gitPromptScript then "$(__git_ps1)" else ""

-- Ideas:
--
-- * Run this application as a service. Output must be made available to Bash
--   atomically.
-- * Track dependencies and only when something changes produce new output.
-- * Track changes in genbashrc so that output is recompiled when new version
--   is deployed.

main :: IO ()
main = getArgs >>= \case
    [] -> main' Lazy.Text.putStr
    [outputFileName] ->
        -- TODO: Make output writing an atomic operation.
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

        onJust lesspipeCommand evalLesspipe

        pathUpdated <- updatePath Prepend $ PathElements
            [ guard (not userBinDirInPath) *> userBinDir
            , guard (not userLocalBinDirInPath) *> userLocalBinDir
            ]
        exportPathIf pathUpdated

        setPrompt ctx

        editor
            [ if haveNeovim then Just "nvim" else Nothing
            , if haveVim then Just "vim" else Nothing
            ]

        aliases ctx

        onJust bashCompletionScript $ \script ->
            bashIfThen "! shopt -oq posix" $ do
                () <- source_ script
                onJust stackBin $ \stack ->
                    when ((stack `isInDir` home) || (macOs `isOs` currentOs))
                        Utils.stackBashCompletion

        when haveGit $ onJust gitPromptScript source

        Utils.fzfConfig fzfBashrc

onJust :: Applicative f => Maybe a -> (a -> f ()) -> f ()
onJust = for_
