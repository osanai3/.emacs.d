# shellcheck shell=bash
export EDITOR="emacsclient --tty --alternate-editor="
export MANWIDTH=80
export CLICOLOR=""
export LSCOLORS="gxfxcxdxbxegedabagacad"
export PATH="~/go/bin:$PATH"

if [ -f ~/.bashrc ] ; then
    # shellcheck source=/dev/null
    . ~/.bashrc
fi
