{-# LANGUAGE BlockArguments #-}
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
import Data.Bool (Bool(False), (&&), (||), not, otherwise)
import Data.Eq (Eq)
import Data.Foldable (for_)
import Data.Function (($), (.))
import Data.Functor ((<$>), (<&>))
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (Maybe(Just, Nothing), isJust, maybe)
import Data.Monoid (Monoid, (<>), mempty)
import Data.Proxy (Proxy(Proxy))
import Data.String (fromString)
import System.Environment (getArgs, getProgName, lookupEnv)
import System.IO (FilePath, IO)
import Text.Show (Show, show)

import qualified Crypto.Hash.SHA256 as SHA256 (hash)
import qualified Data.ByteString.Base16 as Base16
import Data.String.ToString (toString)
import Data.Text (Text)
import qualified Data.Text.Lazy as Lazy (Text)
import qualified Data.Text.Lazy.IO as Lazy.Text (putStr, writeFile)
import Network.HostName (HostName, getHostName)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

import GenBashrc.Bash
import GenBashrc.Cache (getCacheStdFilePath)
import GenBashrc.FilePath
import GenBashrc.Os
import qualified GenBashrc.Os.Linux as Linux
import GenBashrc.PackageManager
import qualified GenBashrc.SystemInfo as SystemInfo
import qualified GenBashrc.Utils as Utils

--import Debug.Trace (traceShowId)


-- TODO:
--
-- * Track dependencies and only when something changes produce new output.
--   This could be done by Shake and its Oracle functionality.  Think of this
--   as having a cache for 'Context'.
--
-- * Introduce Dhall configuration file so that some tweaks can be done without
--   the need of modifying this file.

main :: IO ()
main = getArgs >>= \case
    [] ->
        main' Lazy.Text.putStr

    ["--cached"] ->
        mainCached Lazy.Text.putStr

    ["--cached", outputFileName] ->
        mainCached (Lazy.Text.writeFile outputFileName)

    [outputFileName] ->
        -- TODO: Make output writing an atomic operation.
        main' (Lazy.Text.writeFile outputFileName)

    _ ->
        error "Usage: genbashrc [--cached] [OUTPUT_FILE]"

main' :: (Lazy.Text -> IO a) -> IO a
main' writeOutput = context >>= writeOutput . genBash . bashrc

mainCached :: (Lazy.Text -> IO a) -> IO a
mainCached writeOutput = do
    appName <- getProgName
    ctx <- context
    -- TODO: Hash of genbashrc executable should be used instead of "1".  That
    -- way if genbashrc changes outdated cache won't be used.
    cache <- getCacheStdFilePath appName "1" (hashContext ctx)
    genBashCached cache (bashrc ctx) >>= writeOutput
  where
    hashContext = toString . Base16.encode . SHA256.hash . fromString . show

-- | All the information we gathered from the system and users environment.
--
-- TODO: Split into multiple data types and provide smart constructor for
-- those.  E.g. data type 'Editors' that contains information about known
-- editors.
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
    , userBinDirInPath :: Bool
    , userLocalBinDir :: Maybe FilePath
    , userLocalBinDir' :: FilePath
    , userLocalBinDirInPath :: Bool
    , bashCompletionScript :: Maybe FilePath
    , gitPromptScript :: Maybe FilePath
    , userDircolors :: Maybe FilePath
    , lesspipeCommand :: Maybe CommandName
    , stackBin :: Maybe FilePath
    , tmuxConfig :: Maybe FilePath
    , fzfBashrc :: Maybe FilePath
    , yx :: Maybe FilePath
    , habit :: Maybe FilePath
    , dhallToBash :: Maybe FilePath
    , dhallToJson :: Maybe FilePath
    , dhallToYaml :: Maybe FilePath
    , dhallToText :: Maybe FilePath
    , nixProfile :: Maybe FilePath
    , nixProfileSourced :: Bool
    , haveDirenv :: Bool
    , haveRipgrep :: Bool
    , ripgrepConfig :: Maybe FilePath
    , haveFd :: Bool
    , haveFdfind :: Bool
    -- ^ Command `fdfind` is the same as `fd` on Debian systems.  This is to
    -- disambiguate it from some other command.
    }
  deriving (Eq, Show)

context :: IO Context
context = do
    currentOs <- SystemInfo.detectOs
    hostname <- getHostName
    home <- getHomeDirectory
    (userBinDir, userBinDir') <- Home ?<</> "bin"
    userBinDirInPath <- dirInPath userBinDir'
    (userLocalBinDir, userLocalBinDir') <- DotLocal ?<</> "bin"
    userLocalBinDirInPath <- dirInPath userLocalBinDir'
    haveSudo <- haveExecutable "sudo"
    haveVim <- haveExecutable "vim"
    haveNeovim <- haveExecutable "nvim"
    haveMplayer <- haveExecutable "mplayer"
    haveXpdfCompat <- haveXpdfCompatExecutable
    haveColorDiff <- haveExecutable "colordiff"
    haveScreen <- haveExecutable "screen"
    haveDircolors <- haveExecutable "dircolors"
    haveXinput <- haveExecutable "xinput"
    haveGit <- haveExecutable "git"
    lesspipeCommand <- Utils.lookupLesspipeCommand
    bashCompletionScript <- Utils.lookupBashCompletionScript
    gitPromptScript <- Utils.lookupGitPromptScript currentOs
    userDircolors <- checkFilesM [Home <</> ".dircolors"]
    haveCdrom <- SystemInfo.haveCdrom
    stackBin <- Utils.lookupStack

    -- Tmux doesn't support XDG Base Directory Specification.  This is used to
    -- work around it.
    tmuxConfig <- checkFilesM [xdgConfig <</> "tmux" </> "tmux.conf"]
    haveTmux <- haveExecutable "tmux"

    fzfBashrc <- Utils.lookupFzfBashrc

    yx <- checkFilesM [Home <</> "bin" </> "yx"]
    habit <- checkFilesM [Home <</> "bin" </> "habit"]
    dhallToBash <- checkFilesM
        [ Home <</> "bin" </> "dhall-to-bash"
        , DotLocal <</> "bin" </> "dhall-to-bash"
        ]
    dhallToJson <- checkFilesM
        [ Home <</> "bin" </> "dhall-to-json"
        , DotLocal <</> "bin" </> "dhall-to-json"
        ]
    dhallToYaml <- checkFilesM
        [ Home <</> "bin" </> "dhall-to-yaml"
        , DotLocal <</> "bin" </> "dhall-to-yaml"
        ]
    dhallToText <- checkFilesM
        [ Home <</> "bin" </> "dhall-to-text"
        , DotLocal <</> "bin" </> "dhall-to-text"
        ]

    -- This needs testing.  We need to make sure that Nix works as expected,
    -- however we don't want it to be too pervasive.
    nixProfile <- checkFilesM [Home <</> ".nix-profile/etc/profile.d/nix.sh"]
    nixProfileSourced <- isJust <$> lookupEnv "NIX_PATH"

    -- Direnv is an environment switcher for the shell. This allows
    -- project-specific environment variables without cluttering the
    -- '~/.profile' file.  <https://direnv.net/>
    haveDirenv <- haveExecutable "direnv"

    haveRipgrep <- haveExecutable "rg"
    ripgrepConfig <- checkFilesM [xdgConfig <</> "ripgrep" </> "ripgreprc"]

    haveFd <- haveExecutable "fd"
    haveFdfind <- haveExecutable "fdfind"

    let -- $ grep "^N: Name=.* Touchpad" /proc/bus/input/devices
        -- N: Name="ELAN1200:00 04F3:3059 Touchpad"
        haveTouchpad = False

        canCloseCdrom = False -- TODO

    pure Context{..}
  where
    orA = liftA2 (||)

    haveXpdfCompatExecutable = haveExecutable "xpdf-compat"
        `orA`
            ( isJust <$> checkFilesM
                [ Home <</> "bin" </> "xpdf-compat"
                , DotLocal <</> "bin" </> "xpdf-compat"
                ]
            )

-- TODO: We could ask `command-wrapper` to give us this list.
--
-- > yx completion --index=0 --subcommand=config -- --dhall
dhallSubcommands :: [(CommandName, BashString)]
dhallSubcommands = suffixes <&> \suffix ->
    let command = "dhall" <> suffix
    in  (CommandName command, BashString ("--" <> command))
  where
    suffixes =
        [ ""
        , "-bash"
        , "-diff"
        , "-exec"
        , "-filter"
        , "-format"
        , "-freeze"
        , "-hash"
        , "-lint"
        , "-repl"
        , "-resolve"
        , "-text"
        ]

aliases :: Context -> Bash ()
aliases Context{..} = do
    Utils.standardAliases Utils.AliasOptions{..}

    whenOs linux currentOs \linuxOs -> do
        Linux.whenDistro_ Linux.notDebianCompat linuxOs
            vimAliasForNeovim

        whenPackageManager_ Linux.apt linuxOs case yx of
            Nothing -> do
                alias "apt"     "'sudo apt'"
                alias "apt-get" "'sudo apt-get'"

            Just _ -> do
                alias "apt"       "'yx apt'"
                alias "apt-cache" "'yx apt'"
                alias "apt-get"   "'yx apt'"
                alias "this"      "'yx this'"
                alias "xpdf"      "'yx xpdf'"

                for_ dhallSubcommands \(subcommand, option) -> do
                    alias subcommand ("'yx config " <> option <> "'")

        when (haveTouchpad && haveXinput) do
            -- TODO: Rewrite following to use xinput.
            alias "touchpad-off" "'synclient TouchpadOff=1'"
            alias "touchpad-on"  "'synclient TouchpadOff=0'"

        when haveCdrom do
            alias "eject" "'eject -d'"
            unless canCloseCdrom $ alias "close" "'eject -d -t'"

        when (haveFdfind && not haveFd) do
            -- On some systems `fd` command is available under the name
            -- `fdfind`.
            alias "fd" "'fdfind'"

    whenOs_ macOs currentOs
        vimAliasForNeovim

    alias "evil" $ if haveSudo then "'sudo su -'" else "'su -'"

    when haveMplayer $ alias "mplayer" "'mplayer -idx'"
    when haveXpdfCompat $ alias "xpdf" "xpdf-compat"

    when haveTmux
        . alias "tmux"
            $ "'TERM=xterm-256color tmux"
            <> maybe "" (\cfg -> " -f \"" <> fromString cfg <> "\"") tmuxConfig
            <> "'"

    when haveRipgrep $ onJust ripgrepConfig \cfg ->
        alias "rg" ("'RIPGREP_CONFIG_PATH=\"" <> fromString cfg <> "\" rg'")

    alias "term-title" "'printf \"\\033]2;%s\\007\"'"
  where
    vimAliasForNeovim = when haveNeovim $ alias "vim" "nvim"

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

    -- Don't store following commands in history:
    histignore ["ls", "pwd"]

setPrompt :: Context -> Bash ()
setPrompt Context{..} = do
    function "__env_ps1" do
        let -- Variable CD_LEVEL indicates how many times we have invoked "yx cd" and
            -- ended up in a subshell.
            yxCd = "${CD_LEVEL:+⟦${CD_LEVEL}⟧}"

            -- Variable YX_ENV_DIR indicates that we are in a local environment
            -- delimited by "$YX_ENV_DIR/.yx-env" file.  Similar thing goes for
            -- DIRENV_DIR, only the file is named "$DIRENV_DIR/.envrc".
            yxEnv = mguard (isJust yx) "${YX_ENV_DIR:+∃}"

            -- TODO: DIRENV_DIR is populated even if environment is not loaded.
            -- In which case it is prefixed with `-` character, making the path
            -- invalid.
            direnv = mguard haveDirenv "${DIRENV_DIR:+∃}"

        line @Text ("echo \"" <> yxCd <> yxEnv <> direnv <> "\"")

    when (isJust nixProfile) do
        function "__nix_shell_ps1"
            $ line @Text "echo \"${IN_NIX_SHELL:+⟪nix⟫}\""

        function "__restore_original_ps1"
            . bashIfThen "[[ \"${PS1}\" != \"${ORIGINAL_PS1}\" ]]"
                $ set "PS1" "\"${ORIGINAL_PS1}\""

        bashIfThen "[[ ! \"${PROMPT_COMMAND}\" =~ \"__restore_original_ps1\" ]]"
            $ set "PROMPT_COMMAND"
                "\"__restore_original_ps1${PROMPT_COMMAND:+;${PROMPT_COMMAND}}\""

    when haveScreen
        . function "__screen_ps1"
            $ line @Text "echo \"${WINDOW:+#${WINDOW}}\""

    prompt (Proxy @'PS1)
        $ "'\\[\\e[90m\\]#"
        <> nixShellPs1
        <> "\\[\\e[37m\\]\\u\\[\\e[90m\\]@\\[\\e[37m\\]\\h"
        <> screenPs1
        <> "\\[\\e[90m\\]:\\[\\e[37m\\]\\W\\[\\e[90m\\]"
        <> gitPs1
        <> "$(__env_ps1)\\[\\e[32m\\] ⊢\\[\\e[22;0m\\] '"

    exportPrompt (Proxy @'PS1)

    set "ORIGINAL_PS1" "\"${PS1}\""
  where
    screenPs1 = mguard haveScreen "$(__screen_ps1)"

    nixShellPs1 = mguard (isJust nixProfile) "\\[\\e[32m\\]$(__nix_shell_ps1)"

    gitPs1 =
        mguard (haveGit && isJust gitPromptScript) "$(__git_ps1 \"\57504%s\")"

bashrc :: Context -> Bash ()
bashrc ctx@Context{..} = do
    -- Report job state changes immediately and don't wait for printing new
    -- prompt.
    shopt Set NotifyOfJobTerminationImmediately

    -- Turn on vi-style line editing interface.
    shopt Set Vi

    -- Use visible bell (if available) instead of audible, which is the
    -- default.
    line @Text "set bell-style visible"

    -- Check the window size after each command and, if necessary, update the
    -- values of LINES and COLUMNS.
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
        [ guard haveNeovim *> Just "nvim"
        , guard haveVim *> Just "vim"
        ]

    aliases ctx

    onJust bashCompletionScript \script ->
        bashIfThen "! shopt -oq posix" do
            () <- source_ script
            onJust stackBin \stack ->
                when ((stack `isInDir` home) || (macOs `isOs` currentOs))
                    Utils.stackBashCompletion

    when haveGit $ onJust gitPromptScript source

    Utils.fzfConfig fzfBashrc
    when (isJust fzfBashrc && (haveFd || haveFdfind)) do
        let fdCommand
              | haveFd = "'fd --type file'"
                -- This is actually the 'haveFdfind' case:
              | otherwise = "'fd --type file'"

        set "FZF_DEFAULT_COMMAND" fdCommand
        set "FZF_CTRL_T_COMMAND" fdCommand

    onJust yx \yxBin -> do
        Utils.sourceCommandWrapperCompletion yxBin []

        Utils.sourceCommandWrapperSubcommandCompletion yxBin "this"
            (pure "this")

        for_ dhallSubcommands \(CommandName command, _) ->
            Utils.sourceCommandWrapperSubcommandCompletion yxBin command
                (pure command)

        Utils.sourceCommandWrapperSubcommandCompletion yxBin "xpdf"
            (pure "xpdf")

        Utils.sourceCommandWrapperSubcommandCompletion yxBin "apt"
            ("apt" :| ["apt-cache", "apt-get"])

        () <- source_ ("<(" <> fromString yxBin <> " env --script)" :: Text)

        -- TODO: This will leave an entry in Bash history.  FZF doesn't; get
        -- inspired.
        line @Text ("bind '\"\\C-f\":\"" <> fromString yxBin <> " cd --shell\\n\"'")
        line @Text ("bind '\"\\C-k\":\"" <> fromString yxBin <> " cd\\n\"'")

        -- When CTRL+f is pressed in normal mode then switch to insert mode and
        -- call it there.
        line @Text "bind -m vi-command '\"\\C-f\": \"i\\C-f\"'"

    onJust habit \habitBin -> do
        alias "hb" "habit"
        Utils.sourceCommandWrapperCompletion habitBin ["hb"]

    when haveDirenv
        $ source_ ("<(direnv hook bash)" :: Text)

    unless nixProfileSourced
        $ onJust nixProfile source_

    onJust dhallToBash Utils.sourceOptparseCompletion
    onJust dhallToJson Utils.sourceOptparseCompletion
    onJust dhallToYaml Utils.sourceOptparseCompletion
    onJust dhallToText Utils.sourceOptparseCompletion

onJust :: Applicative f => Maybe a -> (a -> f ()) -> f ()
onJust = for_

mguard :: Monoid a => Bool -> a -> a
mguard p a = if p then a else mempty
