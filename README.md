# genbashrc

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
