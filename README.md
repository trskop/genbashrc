# Genbashrc

## Description

Generate `.bashrc` using Haskell EDSL.

Most conditions, e.g. file existence checks, can be resolved in the generator,
therefore, the result will be a very simple and optimised `.bashrc` for a
particular environment.


## Usage

Generate `.bashrc_generated` optimised for current machine and source it:

```Bash
# ~/.bashrc: executed by bash(1) for non-login shells.

# If not running interactively, don't do anything.
case $- in
    *i*) ;;
      *) return;;
esac

[ -x ~/bin/genbashrc ] && ~/bin/genbashrc ~/.bashrc_generated
[ -r ~/.bashrc_generated ] && . ~/.bashrc_generated
```
