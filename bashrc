alias e='emacsclient -n'
DATETIME="\[\e[0;32m\]\D{%F(%a) %T}\[\e[m\]"
CWD="\[\e[0;33m\]\w\[\e[m\]"
GIT_BRANCH="\[\e[0;36m\]\$(git symbolic-ref --short HEAD 2> /dev/null)\[\e[m\]"
GIT_DIFF="\[\e[0;31m\]\$(if git rev-parse --show-toplevel > /dev/null 2>&1; then git diff --quiet || echo '*'; fi)\[\e[m\]"
GIT_DIFF_CACHED="\[\e[0;32m\]\$(if git rev-parse --show-toplevel > /dev/null 2>&1; then git diff --cached --quiet || echo '*'; fi)\[\e[m\]"
export PS1="$DATETIME $CWD $GIT_BRANCH $GIT_DIFF$GIT_DIFF_CACHED\n$ "
trap 'export PIPE_TO_EMACSCLIENT_COMMAND="$BASH_COMMAND"' DEBUG

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
    READLINE_POINT=$(($READLINE_POINT + $COUNT))
}
bind -m emacs -x '"\C-u": _xdiscard'
bind -m emacs -x '"\C-k": _xkill'
bind -m emacs -x '"\C-y": _xyank'
