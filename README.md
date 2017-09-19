# Genbashrc

## Description

Generate `.bashrc` using Haskell EDSL.

Most conditions, e.g. file existence checks, can be resolved in the generator,
therefore, the result will be a very simple and optimised `.bashrc` for a
particular environment.


## Install

```Bash
git clone git@github.com:trskop/genbashrc.git ~/.local/src/trskop/genbashrc
cd ~/.local/src/trskop/genbashrc
stack install
```

## Usage

Generate `.bashrc_generated` optimised for current machine and source it:

```Bash
cp ~/.bashrc{,~}
cat > ~/.bashrc <<EOF
# ~/.bashrc: executed by bash(1) for non-login shells.

# If not running interactively, don't do anything.
case $- in
    *i*) ;;
      *) return;;
esac

[ -x ~/.local/bin/genbashrc ] && ~/.local/bin/genbashrc ~/.bashrc_generated
[ -r ~/.bashrc_generated ] && . ~/.bashrc_generated
EOF
```
