-- |
-- Module:      Main
-- Description: Generate contents of user's ~/.bashrc
-- Copyright:   (c) 2017-2021 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensible; POSIX.
--
-- Generate contents of user's @~/.bashrc@.
module Main (main)
  where

import Prelude (error)

import Control.Applicative (Applicative, (*>), liftA2, pure)
import Control.Monad ((>>=), guard, unless, when)
import Data.Bool (Bool(False), (&&), (||), not, otherwise)
import Data.Eq (Eq)
import Data.Foldable (and, for_, or)
import Data.Function (($), (.))
import Data.Functor ((<$>), (<&>))
import qualified Data.List as List (isPrefixOf, notElem, take)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (Maybe(Just, Nothing), isJust, isNothing, maybe)
import Data.Monoid (Monoid, (<>), mempty)
import Data.Proxy (Proxy(Proxy))
import Data.String (String, fromString)
import System.Environment (getArgs, getProgName, lookupEnv)
import System.IO (FilePath, IO)
import Text.Show (Show, show)

import qualified Crypto.Hash.SHA256 as SHA256 (hash)
import qualified Data.ByteString.Base16 as Base16
import Data.String.ToString (toString)
import Data.Text (Text)
import qualified Data.Text as Text (unwords)
import qualified Data.Text.Lazy as Lazy (Text)
import qualified Data.Text.Lazy.IO as Lazy.Text (putStr, writeFile)
import Network.HostName (HostName, getHostName)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>), takeFileName)

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
    , mplayerConfigDir :: Maybe FilePath
    , haveXpdfCompat :: Bool
    , haveColorDiff :: Bool
    , haveScreen :: Bool
    , haveTmux :: Bool
    , haveNeovim :: Bool
    , haveNeovimRemote :: Bool
    , haveVim :: Bool
    , haveDircolors :: Bool
    , dircolorsSourced :: Bool
    , userDircolors :: Maybe FilePath
    , haveXinput :: Bool
    , haveCdrom :: Bool
    , haveGit :: Bool
    , canCloseCdrom :: Bool
    , home :: FilePath
    , userBinDir :: Maybe FilePath
    , userBinDir' :: FilePath
    , userBinDirInPath :: Bool
    , userBinDirIsUserLocalBinDir :: Bool
    -- ^ Is @~\/.bin@ a symbolic link pointing to the same destination as
    -- @~\/.local\/bin@?
    , userLocalBinDir :: Maybe FilePath
    , userLocalBinDir' :: FilePath
    , userLocalBinDirInPath :: Bool
    , bashCompletionScript :: Maybe FilePath
    , gitPromptScript :: Maybe FilePath
    , lesspipeCommand :: Maybe CommandName
    , lesspipeSourced :: Bool
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
    -- ^ Is `direnv` executable in `PATH`?
    , haveRipgrep :: Bool
    -- ^ Have `rg` executable in `PATH`?  Ripgrep is a smart recursive grep
    -- implementation <https://github.com/BurntSushi/ripgrep>.
    , ripgrepConfig :: Maybe FilePath
    -- ^ Ripgrep can be configured via `RIPGREP_CONFIG_PATH` environment
    -- variable, this will store a location of a viable configuration file, or
    -- 'Nothing' otherwise.
    , haveFd :: Bool
    -- ^ Is `fd` executable in `PATH`?
    --
    -- <https://github.com/sharkdp/fd>
    , haveFdfind :: Bool
    -- ^ Command `fdfind` is the same as `fd` on Debian systems.  This is to
    -- disambiguate it from some other commands that use the same name.  See
    -- also 'haveFd'.
    , haveYank :: Bool
    -- ^ Is `yank` executable in `PATH`?
    --
    -- <https://github.com/mptre/yank>
    , haveYankCli :: Bool
    -- ^ Command `yank-cli` is the same as `yank` on Debian systems.  This is
    -- to disambiguate it from some other commands that use the same name. See
    -- also 'haveYank'.
    , xdgRuntimeDir :: FilePath
    , haveSensibleBrowser :: Bool
    -- ^ Debian-like diestribution provide utility called `sensible-browser`
    -- which tries to figure out what browser to execute:
    --
    -- 1. If `BROWSER` environment variable is set then use what's there, note
    --    that the variable is not quoted in the script, i.e. more complex
    --    values in shell syntax will work.
    --
    -- 2. If `DISPLAY` and `GNOME_DESKTOP_SESSION_ID` are set then it tries
    --    these browsers in specified order:
    --
    --    * `/usr/bin/gnome-www-browser`
    --    * `/usr/bin/x-www-browser`
    --    * `/usr/bin/www-browser` (in `/usr/bin/gnome-terminal`)
    --
    -- 3. If only `DISPLAY` is set then it will try following:
    --
    --    * `/usr/bin/x-www-browser`
    --    * `/usr/bin/www-browser` (in `/usr/bin/x-terminal-emulator`)
    --
    -- 4. Neither `BROWSER` nor `DISPLAY` are set then it tries to directly run,
    --    without starting terminal first, `/usr/bin/www-www-browser`.
    --
    -- It may be important to note that `/usr/bin/www-browser` is a text-based
    -- browser, e.g. `links`.  Easiest way how to figure out what is set as
    -- `www-browser` is to look at `www-browser(1)` manual page, which is a
    -- symbolic link to a manual page of what is being used as `www-browser`.
    --
    -- For `gnome-www-browser` and `x-www-browser` it is best to just run:
    --
    -- > update-alternatives --display gnome-www-browser
    -- > update-alternatives --display x-www-browser
    --
    -- Or:
    --
    -- > readlink -f $(which gnome-www-browser)
    -- > readlink -f $(which x-www-browser)
    --
    -- To change where `gnome-www-browser` and `x-www-browser` symbolic links
    -- we need to reconfigure alternatives:
    --
    -- > sudo update-alternatives --config gnome-www-browser
    -- > sudo update-alternatives --config x-www-browser
    , haveFirefox :: Bool
    -- ^ Is there a `firefox` executable in the `PATH`?
    , haveFirefoxEsr :: Bool
    -- ^ Is there a `firefox-esr` executable in the `PATH`?  ESR stands for
    -- Extended Support Release.
    , gnomeWwwBrowser :: Maybe FilePath
    -- ^ Is there a `gnome-www-browser` executable in the `PATH`, and where is
    -- it pointing?  'Nothing' means that there is no such executable, and
    -- 'Just' some file path means that the file path is the target of
    -- `gnome-www-browser` symbolic link.  See 'haveSensibleBrowser' for more
    -- information.
    , xWwwBrowser :: Maybe FilePath
    -- ^ Is there a `x-www-browser` executable in the `PATH`, and where is it
    -- pointing?  'Nothing' means that there is no such executable, and 'Just'
    -- some file path means that the file path is the target of `x-www-browser`
    -- symbolic link.  See 'haveSensibleBrowser' for more information.
    , batBin :: Maybe FilePath
    -- ^ Path to @bat@ (alternative to @cat@) executable.
    , terminfoDirsEnv :: Maybe String
    -- ^ Value of @TERMINFO_DIRS@ environment if specified otherwise 'Nothing'.
    , jqColorsEnv :: Maybe String
    -- ^ Value of @JQ_COLORS@ environment if specified otherwise 'Nothing'.
    , fzfDefaultCommandEnv :: Maybe String
    -- ^ Value of @FZF_DEFAULT_COMMAND@ envioronment variable if specified
    -- otherwise 'Nothing'.
    , fzfCtrlTCommandEnv :: Maybe String
    -- ^ Value of @FZF_CTRL_T_COMMAND@ envioronment variable if defined
    -- otherwise 'Nothing'
    }
  deriving stock (Eq, Show)

context :: IO Context
context = do
    -- TODO: We could potentially cache a lot of these values.  What is
    -- troublesome is cache invalidation.  We could potentially hook it into
    -- package manager, but what about Nix?  Other option would be hooking it
    -- up into `yx this`.

    -- This will fail if @HOME@ envioronment is not set and we want that as it
    -- means that we cannot rely on the current environment.
    home <- getHomeDirectory

    -- This needs testing.  We need to make sure that Nix works as expected,
    -- however we don't want it to be too pervasive.
    --
    -- Reason for testing this so early is that some bits may require special
    -- handling if Nix is profile present, but not sourced yet.
    nixProfile <- checkFilesM [Home <</> ".nix-profile/etc/profile.d/nix.sh"]
    nixProfileSourced <- isJust <$> lookupEnv "NIX_PROFILES"

    currentOs <- SystemInfo.detectOs
    hostname <- getHostName
    (userBinDir, userBinDir') <- Home ?<</> "bin"
    userBinDirInPath <- dirInPath userBinDir'
    (userLocalBinDir, userLocalBinDir') <- DotLocal ?<</> "bin"
    userLocalBinDirInPath <- dirInPath userLocalBinDir'
    userBinDirIsUserLocalBinDir <- isSymlinkTo userBinDir' userLocalBinDir'
    haveSudo <- haveExecutable "sudo"
    haveVim <- haveExecutable "vim"
    haveNeovim <- haveExecutable "nvim"
    haveNeovimRemote <- haveExecutable "nvr"
    haveMplayer <- haveExecutable "mplayer"
    mplayerConfigDir <- checkDirsM [xdgConfig <</> "mplayer"]
    haveXpdfCompat <- haveXpdfCompatExecutable
    haveColorDiff <- haveExecutable "colordiff"
    haveScreen <- haveExecutable "screen"

    haveDircolors <- haveExecutable "dircolors"
    dircolorsSourced <- isJust <$> lookupEnv "LS_COLORS"
    -- Command `dircolors` loads user configuration only on certain
    -- distributions, vanilla version doesn't do this. See dir_colors(5) for
    -- more information.
    userDircolors <- checkFilesM
        [ xdgConfig <</> "dircolors" </> "config" -- Non-standard
        , Home <</> ".dircolors" -- Non-standard, but common
        , Home <</> ".dir_colors"
        ]

    haveXinput <- haveExecutable "xinput"
    haveGit <- haveExecutable "git"

    lesspipeCommand <- Utils.lookupLesspipeCommand
    lesspipeSourced <- do
        lessOpen <- lookupEnv "LESSOPEN"
        lessClose <- lookupEnv "LESSCLOSE"
        pure (isJust lessOpen && isJust lessClose)

    bashCompletionScript <- Utils.lookupBashCompletionScript
    gitPromptScript <- Utils.lookupGitPromptScript currentOs
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

    -- Direnv is an environment switcher for the shell. This allows
    -- project-specific environment variables without cluttering the
    -- '~/.profile' file.  <https://direnv.net/>
    haveDirenv <- haveExecutable "direnv"

    haveRipgrep <- haveExecutable "rg"
    ripgrepConfig <- checkFilesM [xdgConfig <</> "ripgrep" </> "ripgreprc"]

    haveFd <- haveExecutable "fd"
    haveFdfind <- haveExecutable "fdfind"

    haveYank <- haveExecutable "yank"
    haveYankCli <- haveExecutable "yank-cli"

    xdgRuntimeDir <- userDir XdgRuntime ""

    haveSensibleBrowser <- haveExecutable "sensible-browser"
    haveFirefox <- haveExecutable "firefox"
    haveFirefoxEsr <- haveExecutable "firefox-esr"
    gnomeWwwBrowser <- findExecutableAndFollowLinks "gnome-www-browser"
    xWwwBrowser <- findExecutableAndFollowLinks "x-www-browser"

    -- Bat can be used as pager, but if it was installed via Nix it will
    -- require special handling due to possible discrepancies in terminfo.
    batBin <- do
        if isJust nixProfile && not nixProfileSourced
            then do
                -- Nix profile will be sourced later and then
                -- $HOME/.nix-profile/bin will be the first entry in the @PATH@,
                -- hence checking for it before calling
                -- 'findExecutableAndFollowLinks'.
                nixBatBin <- checkFilesM [Home <</> ".nix-profile/bin/bat"]
                maybe (findExecutableAndFollowLinks "bat") readlinkRecursive
                    nixBatBin
            else
                findExecutableAndFollowLinks "bat"

    terminfoDirsEnv <- lookupEnv "TERMINFO_DIRS"
    jqColorsEnv <- lookupEnv "JQ_COLORS"
    fzfDefaultCommandEnv <- lookupEnv "FZF_DEFAULT_COMMAND"
    fzfCtrlTCommandEnv <- lookupEnv "FZF_CTRL_T_COMMAND"

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

        when (haveYankCli && not haveYank) do
            -- On some systems `yank` command is available under the name
            -- `yank-cli`.
            alias "yank" "'yank-cli'"

    whenOs_ macOs currentOs
        vimAliasForNeovim

    when (haveNeovim && haveNeovimRemote) do
        bashIfThen "[[ -n \"${NVIM_LISTEN_ADDRESS:-}\" ]]" do
            alias "vim" "'nvr --nostart'"
            alias "nvim" "'nvr --nostart'"

    alias "evil" if haveSudo then "'sudo su -'" else "'su -'"

    when haveMplayer $ alias "mplayer" "'mplayer -idx'"
    when haveXpdfCompat $ alias "xpdf" "xpdf-compat"

    when haveTmux do
        alias "tmux"
            $ "'TERM=xterm-256color tmux"
            <> maybe "" (\cfg -> " -f \"" <> fromString cfg <> "\"") tmuxConfig
            <> "'"

    when haveRipgrep $ onJust ripgrepConfig \cfg ->
        alias "rg" ("'RIPGREP_CONFIG_PATH=\"" <> fromString cfg <> "\" rg'")

    when (maybe False ("/nix/store/" `List.isPrefixOf`) batBin) do
        -- When `bat` is installed via Nix then we need to make sure it
        -- understands our terminal emulator as that may not have been
        -- installed by Nix and may not be among basic terminfo entries.
        alias "bat" "'\
            \TERMINFO_DIRS=\"/etc/terminfo:/lib/terminfo:/usr/share/terminfo:\"\
            \ bat\
            \'"

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
        let -- Variable CD_LEVEL indicates how many times we have invoked
            -- "yx cd" and ended up in a subshell.
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

    when haveScreen do
        function "__screen_ps1"
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

    unless lesspipeSourced do
        onJust lesspipeCommand evalLesspipe

    pathUpdated <- updatePath Prepend $ PathElements
        [ -- We want to start using ~/.local/bin for everything, but a lot of
          -- configuration still relies on ~/bin. This will allow us to
          -- transition easily and safely.
          guard (not userBinDirInPath && not userBinDirIsUserLocalBinDir)
            *> userBinDir
        , guard (not userLocalBinDirInPath) *> userLocalBinDir
        ]
    exportPathIf pathUpdated

    setPrompt ctx

    editor
        [ guard haveNeovim *> Just "nvim"
        , guard haveVim *> Just "vim"
        ]

    -- We want to use Firefox at whenever possible.  Chrome has the unfortunate
    -- property of being very aggressive when installed.
    do
        let -- Reason for `take 1` is that if we have `firefox` then we would
            -- like all of it to point to `firefox` and not `firefox-esr`.
            -- Also, this way we are basically checking for consistency as
            -- well as if it's a Firefox.
            firefoxes = List.take 1
                $  mguard haveFirefox ["firefox"]
                <> mguard haveFirefoxEsr ["firefox-esr"]

            isNotFirefox path = takeFileName path `List.notElem` firefoxes

            gnomeWwwBrowserIsNotFirefox =
                maybe False isNotFirefox gnomeWwwBrowser

            xWwwBrowserIsNotFirefox = maybe False isNotFirefox xWwwBrowser

            needToDefineBrowserVariable = or
                [ gnomeWwwBrowserIsNotFirefox
                , xWwwBrowserIsNotFirefox
                ]

        when needToDefineBrowserVariable do
            browser
                [ guard haveFirefox *> Just "firefox"
                , guard haveFirefoxEsr *> Just "firefox-esr"
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
    let setFzfEnvVars = and
            [ isJust fzfBashrc
            -- Short-circuit if both environment variables are already set.
            , isNothing fzfDefaultCommandEnv || isNothing fzfCtrlTCommandEnv
            -- We need 'fd' or 'fdfind' (Debian) to be present to configure FZF
            -- to use one of them.
            , haveFd || haveFdfind
            ]
    when setFzfEnvVars do
        let fdCommand
              | haveFd = "'fd --type file'"
                -- This is actually the 'haveFdfind' case:
              | otherwise = "'fdfind --type file'"

        when (isNothing fzfDefaultCommandEnv) do
            setAndExport "FZF_DEFAULT_COMMAND" fdCommand
        when (isNothing fzfCtrlTCommandEnv) do
            setAndExport "FZF_CTRL_T_COMMAND" fdCommand

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

        -- This is based on 'fzf' bindings. They avoid using \e (escape) to
        -- avoid delays in terminal waiting for the next key. To get the same
        -- behaviour they bound \C-x\C-a (CTRL-x CTRL-a). Using the same
        -- bindings as 'fzf' allows us to limit number of bindings that are in
        -- use, however, it also has a downside if 'fzf' bindings decide to
        -- change something and we interfere.
        --
        -- In the addition to the bellow CTRL-m is used which is <CR> AKA new
        -- line.
        bind [] "\\C-x\\C-a" "vi-movement-mode"
        bind [] "\\C-x\\C-e" "shell-expand-line"
        bind [] "\\C-x\\C-r" "redraw-current-line"

        bind [] "\\C-f"
            ( "\"`"
            <> fromString yxBin
            <> " cd --self-command --shell`\\C-x\\C-e\\C-x\\C-r\\C-m\""
            )
        bind [] "\\C-\\M-f"
            ( "\"`"
            <> fromString yxBin
            <> " cd --bash-command`\\C-x\\C-e\\C-x\\C-r\\C-m\""
            )
        bind [] "\\C-k"
            ( "\"`"
            <> fromString yxBin
            <> " cd --self-command`\\C-x\\C-e\\C-x\\C-r\\C-m\""
            )

        -- When CTRL+f or CTRL+ALT+f is pressed in normal mode then switch to
        -- insert mode and call it there.
        bind ["-m", "vi-command"] "\\C-f" "\"i\\C-f\""
        bind ["-m", "vi-command"] "\\C-\\M-f" "\"i\\C-\\M-f\""

    function "__load_habit_completion" do
        line @Text "local -a -r habitAliases=('hb')"
        line @Text "if [[ -n \"${HABIT_BASH_COMPLETION}\" ]]; then"
        line @Text "    source \"${HABIT_BASH_COMPLETION}\""
        line @Text "    for habitAlias in \"${habitAliases[@]}\"; do"
        line @Text "        alias \"${habitAlias}=habit\""
        line @Text "        complete -o filenames -F '_habit' \"${habitAlias}\""
        line @Text "    done"
        line @Text "else"
        line @Text "    complete -p habit &>/dev/null && complete -r habit"
        line @Text "    for habitAlias in \"${habitAliases[@]}\"; do"
        line @Text "        alias \"${habitAlias}\" &>/dev/null && unalias hb"
        line @Text "        complete -p \"${habitAlias}\" &>/dev/null && complete -r \"${habitAlias}\""
        line @Text "    done"
        line @Text "fi"

    bashIfThen "[[ ! \"${PROMPT_COMMAND}\" =~ \"__load_habit_completion\" ]]" do
        bashIf "[[ \"${PROMPT_COMMAND}\" == *\\; ]]"
            ( set "PROMPT_COMMAND"
                "\"${PROMPT_COMMAND}__load_habit_completion\""
            )
            ( set "PROMPT_COMMAND"
                "\"${PROMPT_COMMAND:+${PROMPT_COMMAND};}__load_habit_completion\""
            )

    when haveDirenv do
        source_ ("<(direnv hook bash)" :: Text)

    unless nixProfileSourced do
        onJust nixProfile source_

    onJust dhallToBash Utils.sourceOptparseCompletion
    onJust dhallToJson Utils.sourceOptparseCompletion
    onJust dhallToYaml Utils.sourceOptparseCompletion
    onJust dhallToText Utils.sourceOptparseCompletion

    when (isNothing jqColorsEnv) do
        -- More usable set of colours with dark background:
        setAndExport "JQ_COLORS" "'2;37:0;37:0;37:0;37:0;32:1;37:1;37'"

    -- Some tools are installed via Nix which uses its own terminfo
    -- installation. Unfortunately Kitty terminfo is not part of it, hence some
    -- tools not being able to use it properly. By exporting 'TERMINFO_DIRS'
    -- with a value basically lists standard system paths should give us a
    -- reliable way how to use such tools without breaking anything.
    -- Documentation says that "an empty entry is interpreted as a command to
    -- search system terminfo directory", which should give us the ability to
    -- even include Nix-specific directories.
    when (isNothing terminfoDirsEnv && isJust nixProfile) do
        setAndExport "TERMINFO_DIRS"
            "'/etc/terminfo:/lib/terminfo:/usr/share/terminfo:'"

    onJust mplayerConfigDir \dir ->
        setAndExport "MPLAYER_HOME" (fromString dir)

bind :: [Text] -> Text -> Text -> Bash ()
bind opts key binding = line
    ( "bind "
    <> Text.unwords opts
    <> " '\"" <> key <> "\":" <> binding <> "'"
    )

onJust :: Applicative f => Maybe a -> (a -> f ()) -> f ()
onJust = for_

mguard :: Monoid a => Bool -> a -> a
mguard p a = if p then a else mempty
