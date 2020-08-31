-- |
-- Module:      GenBashrc.Bash
-- Description: Monad for generating Bash scripts.
-- Copyright:   (c) 2017-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensible; POSIX.
--
-- Monad for generating Bash scripts.
module GenBashrc.Bash
--  (
--  )
  where

import Prelude ((-), (+), error, fromIntegral)

import Control.Applicative (Alternative, Applicative, (*>), (<|>), empty, pure)
import Control.Monad (Monad, MonadPlus, guard, replicateM_, when)
import Data.Bool (Bool(False, True))
import Data.Char (Char)
import Data.Coerce (coerce)
import Data.Eq (Eq, (/=), (==))
import Data.Foldable (Foldable, foldMap)
import Data.Function (($), (.), flip, id)
import Data.Functor (Functor, (<$))
import qualified Data.List as List (break, intersperse, map, nub)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, maybe)
import Data.Monoid (Alt(Alt, getAlt), Monoid(mempty, mappend), (<>), mconcat)
import Data.Ord (Ord)
import Data.Proxy (Proxy)
import Data.Semigroup (Semigroup)
import qualified Data.Semigroup as Semigroup ((<>))
import Data.String (IsString, String, fromString)
import Data.Traversable (Traversable)
import Data.Word (Word)
import System.IO (FilePath, IO)
import Text.Show (Show(showsPrec), show)

import Control.Monad.State (StateT, evalStateT, modify, state)
import Control.Monad.Writer (WriterT, execWriterT, tell)
import Data.Text (Text)
import qualified Data.Text as Text (intercalate, null, singleton)
import qualified Data.Text.Lazy as Lazy (Text)
import qualified Data.Text.Lazy as Lazy.Text (fromStrict)
import qualified Data.Text.Lazy.Builder as Text (Builder)
import qualified Data.Text.Lazy.Builder as Text.Builder
    ( fromLazyText
    , fromText
    , singleton
    , toLazyText
    )
import System.FilePath.Posix (searchPathSeparator)

import GenBashrc.Cache (cached)

data BashContext = BashContext
    { indentLevel :: Word
    , indentStep :: Text.Builder
    , inLine :: Bool
    }

-- TODO: Keep track of indentation using either ReaderT or StateT.
newtype Bash a = Bash
    { runBash :: StateT BashContext (WriterT Text.Builder Maybe) a
    }
  deriving newtype (Alternative, Applicative, Functor, Monad, MonadPlus)

class (IsString t, Monoid t, Semigroup t) => Bashable t where
    toTextBuilder :: t -> Text.Builder
    fromText :: Text -> t

instance Bashable Text.Builder where
    toTextBuilder = id
    fromText = Text.Builder.fromText

instance Bashable Text where
    toTextBuilder = Text.Builder.fromText
    fromText = id

instance Bashable Lazy.Text where
    toTextBuilder = Text.Builder.fromLazyText
    fromText = Lazy.Text.fromStrict

class Bashable t => BashableStr t where
    str :: Text -> t
    str = fromText

instance BashableStr Text where
    str = id

class Bashable t => BashableVar t where
    var :: Text -> t
    var = fromText

newtype CommandName = CommandName {getCommandName :: Text}
  deriving newtype
    ( Bashable
    , BashableStr
    , CmdArg
    , Eq
    , IsString
    , Monoid
    , Ord
    , Semigroup
    )

noop :: CommandName
noop = ":"

instance Show CommandName where
    showsPrec d (CommandName t) = showsPrec d t

newtype VariableName = VariableName {getVariableName :: Text}
  deriving newtype (Bashable, Eq, IsString, Monoid, Ord, Semigroup)

instance Show VariableName where
    showsPrec d (VariableName t) = showsPrec d t

instance BashableVar VariableName where
    var = fromText

newtype BashString = BashString {getBashString :: Text}
  deriving newtype (Bashable, BashableStr, Eq, IsString, Monoid, Ord, Semigroup)

instance Show BashString where
    showsPrec d (BashString t) = showsPrec d t

instance BashableVar a => Bashable (Expand a) where
    toTextBuilder (Expand a) = "${" <> toTextBuilder a <> "}"
    fromText = var

newtype Expand a = Expand a
  deriving newtype (Eq, Monoid, Ord, Semigroup)

instance IsString a => IsString (Expand a) where
    fromString = \case
        '$' : '{' : s ->
            case List.break (== '}') s of
                (name, "}") -> Expand $ fromString name
                (_, "") -> errorNonTerminatedExpansion
                _ -> errorTrailingCharacters
        '$' : s -> Expand $ fromString s
        _ -> errorNotAnExpansion
      where
        errorNonTerminatedExpansion =
            error "Missing '}' at the end of '${...}'."

        errorTrailingCharacters =
            error "Trailing characters after '${...}'."

        errorNotAnExpansion =
            error "Not an expansion (either '$...' or '${...}')."

instance BashableVar a => BashableVar (Expand a) where
    var = Expand . var

exVar :: Text -> Expand VariableName
exVar = var

genBash :: Bash () -> Lazy.Text
genBash = Text.Builder.toLazyText . fromMaybe mempty . execWriterT
    . flip evalStateT initBashContext . runBash
  where
    initBashContext = BashContext
        { indentLevel = 0
        , indentStep = "    "
        , inLine = False
        }

genBashCached :: FilePath -> Bash () -> IO Lazy.Text
genBashCached cacheFile = cached cacheFile genBash

startLineIfNotAlready :: Bash ()
startLineIfNotAlready = Bash do
    (lvl, step) <- state \ctx@BashContext{indentLevel, indentStep, inLine} ->
        if inLine
            then ((0, indentStep), ctx)
            else ((fromIntegral indentLevel, indentStep), ctx{inLine = True})
    replicateM_ lvl $ tell step

incIndentLevel :: Bash ()
incIndentLevel =
    Bash . modify $ \ctx@BashContext{indentLevel} ->
        ctx{indentLevel = indentLevel + 1}

decIndentLevel :: Bash ()
decIndentLevel = Bash . modify $ \ctx@BashContext{indentLevel} -> ctx
    { indentLevel =
        if indentLevel == 0
            then indentLevel
            else indentLevel - 1
    }

endLineIfNotAlready :: Bash ()
endLineIfNotAlready =
    Bash . modify $ \ctx@BashContext{inLine} ->
        if inLine
            then ctx{inLine = False}
            else ctx

text :: Bashable txt => txt -> Bash ()
text txt = do
    startLineIfNotAlready
    Bash . tell $ toTextBuilder txt

char :: Char -> Bash ()
char ch = do
    startLineIfNotAlready
    Bash . tell $ Text.Builder.singleton ch

eol :: Bash ()
eol = do
    char '\n'
    endLineIfNotAlready

line :: Bashable txt => txt -> Bash ()
line t = do
    text t
    eol

setNoEol' :: (Bashable lhs, Bashable rhs) => lhs -> rhs -> Bash ()
setNoEol' name value = do
    text name
    char '='
    text value

set' :: (Bashable lhs, Bashable rhs) => lhs -> rhs -> Bash ()
set' name value = do
    setNoEol' name value
    eol

setNoEol :: VariableName -> BashString -> Bash ()
setNoEol = setNoEol'

-- | @export NAME[=VALUE]@
set :: VariableName -> BashString -> Bash ()
set = set'

-- | @export NAME[=VALUE]@
export' :: (Bashable lhs, Bashable rhs) => lhs -> Maybe rhs -> Bash ()
export' name possiblyValue = do
    text @Text "export "
    maybe (line name) (set' name) possiblyValue

-- | @export NAME[=VALUE]@
export :: VariableName -> Maybe BashString -> Bash ()
export = export'

-- | @NAME=VALUE; export NAME@
setAndExport :: VariableName -> BashString -> Bash ()
setAndExport name value = do
    setNoEol name value
    text @Text "; "
    export name Nothing

-- | @alias COMMAND_NAME=STRING@
alias :: CommandName -> BashString -> Bash ()
alias name value = do
    text @Text "alias "
    set' name value

-- | @eval STRING@
evalNoEol' :: Bashable str => str -> Bash ()
evalNoEol' s = do
    text @Text "eval "
    text s

-- | @eval STRING@
eval' :: Bashable str => str -> Bash ()
eval' s = do
    evalNoEol' s
    eol

-- | @eval STRING@
evalNoEol :: BashString -> Bash ()
evalNoEol = evalNoEol'

-- | @eval STRING@
eval :: BashString -> Bash ()
eval = eval'

-- | @# COMMENT@
comment :: Text -> Bash ()
comment msg = do
    text @Text "# "
    line msg

bashIf :: BashString -> Bash () -> Bash () -> Bash ()
bashIf cond onThen onElse = do
    text @Text "if "
    text cond
    line @Text "; then"
    incIndentLevel
    onThen <|> cmd noop
    elseBranch <|> pure ()
    decIndentLevel
    line @Text "fi"
  where
    elseBranch = do
        decIndentLevel
        line @Text "else"
        incIndentLevel
        onElse

bashIfThen :: BashString -> Bash () -> Bash ()
bashIfThen cond onThen = bashIf cond onThen empty

type FunctionName = Text    -- TODO

function :: FunctionName -> Bash () -> Bash ()
function name body = do
    line $ "function " <> name <> "() {"
    incIndentLevel
    body <|> cmd noop
    decIndentLevel
    line @Text "}"

-- {{{ Prompt -----------------------------------------------------------------

data PromptType
    = PS1
    -- ^ PS1 is the primary prompt which is displayed before each command, thus
    -- it is the one most people customize.

    | PS2
    -- ^ PS2 is the secondary prompt displayed when a command needs more input
    -- (e.g. a multi-line command).

    | PS3
    -- ^ PS3 is not very commonly used. It is the prompt displayed for Bash's
    -- @select@ built-in which displays interactive menus. Unlike the other
    -- prompts, it does not expand Bash escape sequences. Usually you would
    -- customize it in the script where the @select@ is used rather than in
    -- your @.bashrc@.

    | PS4
    -- ^ PS4 is also not commonly used. It is displayed when debugging Bash
    -- scripts to indicate levels of indirection. The first character is
    -- repeated to indicate deeper levels.

type PromptSequence = Text  -- TODO

type family Prompt (t :: PromptType) where
    Prompt 'PS1 = PromptSequence
    Prompt 'PS2 = PromptSequence
    Prompt 'PS3 = Text
    Prompt 'PS4 = PromptSequence

class SetPrompt (t :: PromptType) where
    prompt :: Proxy t -> Prompt t -> Bash ()
    exportPrompt :: Proxy t -> Bash ()

instance SetPrompt 'PS1 where
    prompt _ = set' @Text "PS1"
    exportPrompt _ = export' @Text @PromptSequence "PS1" Nothing

instance SetPrompt 'PS2 where
    prompt _ = set' @Text "PS2"
    exportPrompt _ = export' @Text @PromptSequence "PS2" Nothing

instance SetPrompt 'PS3 where
    prompt _ = set' @Text "PS3"
    exportPrompt _ = export' @Text @Text "PS3" Nothing

instance SetPrompt 'PS4 where
    prompt _ = set' @Text "PS4"
    exportPrompt _ = export' @Text @PromptSequence "PS4" Nothing

-- }}} Prompt -----------------------------------------------------------------

-- {{{ Shopt ------------------------------------------------------------------

data SetOrUnset = Set | Unset
  deriving stock (Eq, Show)

data Shopt
    = Checkwinsize
    -- ^ @shopt {-s|-u} checkwinsize@
    | Histappend
    -- ^ @shopt {-s|-u} histappend@
    | Vi
    -- ^ @set {-o|+o} vi@
    | NotifyOfJobTerminationImmediately
    -- ^ @set {-b|+b}@
  deriving stock (Eq, Show)

shopt :: SetOrUnset -> Shopt -> Bash ()
shopt setOrUnset = \case
    Checkwinsize -> shoptCmd "checkwinsize"
    Histappend -> shoptCmd "histappend"
    Vi -> setCmd 'o' "vi"
    NotifyOfJobTerminationImmediately -> setCmd 'b' ""
  where
    setCmd opt arg = do
        text @Text "set "
        char case setOrUnset of
            Set -> '-'
            Unset -> '+'
        char opt
        if Text.null arg
            then eol
            else do
                char ' '
                line @Text arg

    shoptCmd arg = do
        text @Text "shopt "
        text @Text case setOrUnset of
            Set -> "-s"
            Unset -> "-u"
        char ' '
        line @Text arg

-- }}} Shopt ------------------------------------------------------------------

-- {{{ Command ----------------------------------------------------------------

class CmdArgs a where
    type CmdResult a :: *
    cmdArgs :: Maybe Text.Builder -> Text.Builder -> a

instance CmdArgs (Bash ()) where
    type CmdResult (Bash ()) = Bash ()
    cmdArgs rest accum = maybe (line accum) (text . (accum <>)) rest

instance (CmdArg a, CmdArgs t) => CmdArgs (a -> t) where
    type CmdResult (a -> t) = CmdResult t
    cmdArgs rest accum arg =
        cmdArgs rest (accum <> Text.Builder.singleton ' ' <> cmdArg arg)

class CmdArg a where
    cmdArg :: a -> Text.Builder

instance CmdArg Text.Builder where
    cmdArg = id

instance CmdArg Text where
    cmdArg = Text.Builder.fromText

instance CmdArg Lazy.Text where
    cmdArg = Text.Builder.fromLazyText

instance CmdArg String where
    cmdArg = fromString

type (:->) args r = CmdResult args ~ r => args

cmd :: forall r args. CmdArgs args => CommandName -> args :-> r
cmd name = cmdArgs Nothing (cmdArg name)

source :: forall r args. CmdArgs args => args :-> r
source = cmd "."

source_ :: forall args. CmdArgs args => args :-> Bash ()
source_ = source

-- | @expand "foo" ~> $(foo)@
expand
    :: forall r args
    . (CmdArgs args, CmdResult args ~ r)
    => CommandName
    -> args :-> r
expand name = cmdArgs (Just ")") (cmdArg $ "$(" <> name)

-- }}} Command ----------------------------------------------------------------

-- {{{ Common -----------------------------------------------------------------

data PrependOrAppend = Prepend | Append
  deriving stock (Eq, Show)

updateVar
    :: VariableName
    -> BashString
    -- ^ Separator string.
    -> PrependOrAppend
    -> BashString
    -- ^ Value to prepend/append.
    -> Bash ()
updateVar name@(VariableName rawName) sep updateType value =
    set' name case updateType of
        Prepend ->
            "\"" <> value <> sep <> "${" <> BashString rawName <> "}\""
        Append ->
            "\"${" <> BashString rawName <> "}" <> sep <> value <> "\""

newtype PathString = PathString {getPathString :: Text}
  deriving newtype (Eq, IsString, Show)

instance Semigroup PathString where
    "" <>  s = s
    s  <> "" = s
    s1 <> s2 = coerce (\s1' s2' -> s1' <> pathSep <> s2' :: Text) s1 s2
      where
        pathSep = Text.singleton searchPathSeparator

instance Monoid PathString where
    mempty = ""
    mappend = (Semigroup.<>)

class IsPathString a where
    toPathString :: a -> PathString

    onPathString :: Alternative f => (PathString -> f r) -> a -> f r
    onPathString f a = guard (s /= "") *> f s
      where
        s = toPathString a

instance IsPathString PathString where
    toPathString = id

instance IsPathString BashString where
    toPathString = coerce

instance IsPathString FilePath where
    toPathString = fromString

instance IsPathString a => IsPathString (Maybe a) where
    toPathString = maybe "" toPathString
    onPathString = maybe empty . onPathString

newtype PathElements t a = PathElements {getPathElements :: t a}
  deriving stock (Eq, Foldable, Functor, Show, Traversable)
  deriving newtype (Alternative, Applicative, Monad)

instance (Foldable t, IsPathString a) => IsPathString (PathElements t a) where
    toPathString (PathElements ts) = foldMap toPathString ts

updatePath' :: PrependOrAppend -> PathString -> Bash ()
updatePath' op = updateVar "PATH" "${PATH:+:}" op . coerce

updatePath :: IsPathString a => PrependOrAppend -> a -> Bash Bool
updatePath op a = (True <$ onPathString (updatePath' op) a) <|> pure False

updatePath_ :: IsPathString a => PrependOrAppend -> a -> Bash ()
updatePath_ op a = onPathString (updatePath' op) a <|> pure ()

exportPathIf :: Bool -> Bash ()
exportPathIf p = when p exportPath

exportPath :: Bash ()
exportPath = export "PATH" Nothing

evalLesspipe :: CommandName -> Bash ()
evalLesspipe c = eval' $ "\"$(SHELL=/bin/sh " <> c <> ")\""

-- | Set @EDITOR@ and @VISUAL@ environment variables to specified values.
setEditor :: BashString -> Bash ()
setEditor e = do
    set "EDITOR" e
    export "EDITOR" Nothing
    set "VISUAL" e
    export "VISUAL" Nothing

-- | Set @EDITOR@ and @VISUAL@ environment variables to first non-@Nothing@
-- value.
editor :: [Maybe BashString] -> Bash ()
editor = getAlt . foldMap (maybe mempty $ Alt . setEditor)

-- | Set @BROWSER@ environment varaible to the specified value.
setBrowser :: BashString -> Bash ()
setBrowser e = do
    set "BROWSER" e
    export "BROWSER" Nothing

-- | Set @BROWSER@ environment variable to first non-@Nothing@ value.
browser :: [Maybe BashString] -> Bash ()
browser = getAlt . foldMap (maybe mempty $ Alt . setBrowser)

withDircollorsWhen :: Bool -> Maybe FilePath -> Bash () -> Bash ()
withDircollorsWhen condition userDircolors actions = when condition do
    evalDircolors userDircolors
    actions

evalDircolors :: Maybe FilePath -> Bash ()
evalDircolors userDircolors =
    eval $ "\"$(dircolors -b" <> cfg <> ")\""
  where
    cfg = maybe "" ((" " <>) . fromString) userDircolors

-- }}} Common -----------------------------------------------------------------

-- {{{ History ----------------------------------------------------------------

data HistControlAtrr
    = HistIgnorespace
    | HistIgnoredups
    | HistErasedups
  deriving (Eq, Show)

histignoreboth :: [HistControlAtrr]
histignoreboth = [HistIgnorespace, HistIgnoredups]

histcontrol :: Maybe PrependOrAppend -> [HistControlAtrr] -> Bash ()
histcontrol op = apply op . fromAtrrs
  where
    name = "HISTCONTROL" :: IsString s => s
    sep = "${" <> name <> ":+,}"
    apply = maybe (set name) (updateVar name sep)
    fromAtrrs = mconcat . List.intersperse "," . List.map fromAtrr . List.nub

    fromAtrr = \case
        HistIgnorespace -> "ignorespace"
        HistIgnoredups  -> "ignoredups"
        HistErasedups   -> "erasedups"

histsize :: Maybe Word -> Bash ()
histsize = set "HISTSIZE" . maybe "" (fromString . show)

histfilesize :: Maybe Word -> Bash ()
histfilesize = set "HISTFILESIZE" . maybe "" (fromString . show)

histignore :: [BashString] -> Bash ()
histignore =
    set' ("HISTIGNORE" :: VariableName) . Text.intercalate ":" . coerce

-- }}} History ----------------------------------------------------------------
