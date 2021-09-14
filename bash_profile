# shellcheck shell=bash
export EDITOR=emacsclient
export MANWIDTH=80
export CLICOLOR=""
export LSCOLORS="gxfxcxdxbxegedabagacad"

if [ -f ~/.bashrc ] ; then
    # shellcheck source=/dev/null
    . ~/.bashrc
fi
