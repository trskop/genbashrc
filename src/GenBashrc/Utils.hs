{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module:      GenBashrc.Utils
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO: Module description.
module GenBashrc.Utils
    (
    -- * Bash Completion
      lookupBashCompletionScript

    -- * Standard Aliases
    , AliasOptions(..)
    , standardAliases

    -- * Git
    , lookupGitPromptScript

    -- * Less
    , lookupLesspipeCommand

    -- * Stack
    --
    -- | Haskell build tool Stack.
    , lookupStack
    , stackBashCompletion
    )
  where

import Control.Applicative (pure)
import Control.Monad ((>>=), when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bool (Bool, bool)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.String (fromString)
import System.IO (FilePath)

import System.Directory (findExecutable)
import System.FilePath ((</>))

import GenBashrc.Bash (Bash, CommandName, alias, eval, withDircollorsWhen)
import GenBashrc.Os (OsInfo(Linux, MacOs), linux, macOs, whenOs_)
import GenBashrc.FilePath
    ( UserDirectory(DotLocal, Home)
    , (<</>)
    , checkFiles
    , checkFilesM
    , haveExecutable
    )


lookupBashCompletionScript :: MonadIO io => io (Maybe FilePath)
lookupBashCompletionScript = checkFiles
    [ "/usr/share/bash-completion/bash_completion"
    , "/etc/bash_completion"
    , "/usr/local/etc/bash_completion"
    ]

lookupGitPromptScript :: MonadIO io => OsInfo -> io (Maybe FilePath)
lookupGitPromptScript = checkFiles . \case
    Linux _ ->
        [ "/usr/lib/git-core/git-sh-prompt"
        , "/etc/bash_completion.d/git-prompt"
        , usrLocal
        ]
    MacOs _ ->
        [ usrLocal
        , "/Applications/Xcode.app/Contents/Developer/usr/share/git-core/git-prompt.sh"
        ]
  where
    usrLocal = "/usr/local/etc/bash_completion.d/git-prompt"

lookupLesspipeCommand :: MonadIO io => io (Maybe CommandName)
lookupLesspipeCommand =
    lookupCommand "lesspipe"
        >>= maybe (lookupCommand "lesspipe.sh") (pure . Just)
  where
    lookupCommand c =
        bool Nothing (Just $ fromString c) <$> haveExecutable c

-- {{{ Stack ------------------------------------------------------------------

lookupStack :: MonadIO io => io (Maybe FilePath)
lookupStack =
    checkHomeBins >>= maybe (liftIO $ findExecutable "stack") (pure . Just)
  where
    checkHomeBins = checkFilesM
      [ Home <</> "bin" </> "stack"
      , DotLocal <</> "bin" </> "stack"
      ]

stackBashCompletion :: Bash ()
stackBashCompletion = eval "\"$(stack --bash-completion-script stack)\""

-- }}} Stack ------------------------------------------------------------------

-- {{{ Aliases ----------------------------------------------------------------

data AliasOptions = AliasOptions
    { currentOs :: OsInfo
    , haveColorDiff :: Bool
    , haveDircolors :: Bool
    , userDircolors :: Maybe FilePath
    }

standardAliases :: AliasOptions -> Bash ()
standardAliases AliasOptions{..} = do
    alias "cp" "'cp -i'"
    alias "mv" "'mv -i'"
    alias "rm" "'rm -i'"

    whenOs_ linux currentOs $ do
        withDircollorsWhen haveDircolors userDircolors $ do
            alias "ls" "'ls --color=auto'"
            alias "dir" "'dir --color=auto'"
            alias "vdir" "'vdir --color=auto'"

            portableColourisedAliases

    whenOs_ macOs currentOs $ do
        alias "ls" "'ls -G'"

        portableColourisedAliases
  where
    portableColourisedAliases = do
        alias "egrep" "'egrep --color=auto'"
        alias "fgrep" "'fgrep --color=auto'"
        alias "grep" "'grep --color=auto'"

        when haveColorDiff
            $ alias "diff" "colordiff"

-- }}} Aliases ----------------------------------------------------------------
