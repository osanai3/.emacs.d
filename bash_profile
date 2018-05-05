alias e='emacsclient -n'
export EDITOR=emacsclient
export MANWIDTH=80
export PAGER="emacs --batch -l ~/.emacs.d/elpa/pipe-to-emacsclient-0.1/pipe-to-emacsclient.el --eval='(pipe-to-emacsclient-batch)'"
DATETIME="\[\e[0;32m\]\D{%F(%a) %T}\[\e[m\]"
CWD="\[\e[0;33m\]\w\[\e[m\]"
GIT_BRANCH="\[\e[0;36m\]\$(git symbolic-ref --short HEAD 2> /dev/null)\[\e[m\]"
export PS1="$DATETIME $CWD $GIT_BRANCH\n$ "
trap 'export PIPE_TO_EMACSCLIENT_COMMAND="$BASH_COMMAND"' DEBUG
export CLICOLOR=""
export LSCOLORS="gxfxcxdxbxegedabagacad"
