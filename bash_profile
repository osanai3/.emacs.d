export EDITOR=emacsclient
export MANWIDTH=80
export PAGER="emacs --batch -l ~/.emacs.d/elpa/pipe-to-emacsclient-0.1/pipe-to-emacsclient.el --eval='(pipe-to-emacsclient-batch)'"
export CLICOLOR=""
export LSCOLORS="gxfxcxdxbxegedabagacad"

if [ -f ~/.bashrc ] ; then
. ~/.bashrc
fi
