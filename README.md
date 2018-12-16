# Genbashrc

## Description

Generate `.bashrc` using Haskell EDSL.

Most conditions, e.g. file existence checks, can be resolved in the generator,
therefore, the result will be a very simple and optimised `.bashrc` for a
particular environment.


### Dependencies

## MacOS

```
brew install haskell-stack
```

Optional:

```
brew install bash bash-completion colordiff lesspipe git neovim
```


## Install

```Bash
git clone git@github.com:trskop/genbashrc.git ~/.local/src/trskop/genbashrc
cd ~/.local/src/trskop/genbashrc
stack install
```


## Usage

Generate `.bashrc` optimised for current machine and source it:

```Bash
cp ~/.bashrc{,~}
cat > ~/.bashrc <<EOF
# ~/.bashrc: executed by bash(1) for non-login shells.

# If not running interactively, don't do anything.
case $- in
    *i*) ;;
      *) return;;
esac

[[ -x "$HOME/.local/bin/genbashrc" ]] && source <("$HOME/.local/bin/genbashrc")
EOF
```
