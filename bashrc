# shellcheck shell=bash
export PAGER="emacs --batch -l ~/.emacs.d/elpa/pipe-to-emacsclient-0.2.1/pipe-to-emacsclient.el --eval='(pipe-to-emacsclient-batch)'"

alias e='emacsclient -n'
alias ep="PIPE_TO_EMACSCLIENT_BUFFER_NAME=*pager* $PAGER"
alias em="osascript -e 'activate application \"Emacs\"'"

DATETIME="\[\e[0;32m\]\D{%F(%a) %T}\[\e[m\]"
CWD="\[\e[0;33m\]\w\[\e[m\]"
git-prompt() {
    if [ "$(git rev-parse --is-inside-work-tree 2> /dev/null)" = true ]
    then
        HEAD="$(git symbolic-ref --short -q HEAD || echo \["$(git name-rev --name-only --always HEAD)"\])"
        printf %b "\001\e[0;36m\002$HEAD\001\e[m\002 "
        git diff --cached --quiet || echo -en '\001\e[0;32m\002*\001\e[m\002'
        test ! -n "$(git ls-files -o --exclude-standard)" && git diff --quiet || printf %b '\001\e[0;31m\002*\001\e[m\002'
    fi
}
export PS1="$DATETIME $CWD \$(git-prompt)\n$ "
trap 'export PIPE_TO_EMACSCLIENT_BUFFER_NAME="*pager*<$BASH_COMMAND>"' DEBUG

_xdiscard() {
    echo -n "${READLINE_LINE:0:$READLINE_POINT}" | pbcopy
    READLINE_LINE="${READLINE_LINE:$READLINE_POINT}"
    READLINE_POINT=0
}
_xkill() {
    echo -n "${READLINE_LINE:$READLINE_POINT}" | pbcopy
    READLINE_LINE="${READLINE_LINE:0:$READLINE_POINT}"
}
_xyank() {
    CLIP=$(pbpaste)
    COUNT=$(echo -n "$CLIP" | wc -c)
    READLINE_LINE="${READLINE_LINE:0:$READLINE_POINT}${CLIP}${READLINE_LINE:$READLINE_POINT}"
    READLINE_POINT=$((READLINE_POINT + COUNT))
}
bind -m emacs -x '"\C-u": _xdiscard'
bind -m emacs -x '"\C-k": _xkill'
bind -m emacs -x '"\C-y": _xyank'

shopt -s globstar

hex() {
    hexdump -v -e '/1 "%02x"'
}

emacs-eval() {
    emacsclient --eval "$(printf '(write-region %s nil "%s" t)' $1 $2)"
}

if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    source "${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh"
    alias e='vterm_cmd find-file'
fi

# Local Variables:
# mode: shell-script
# End:
