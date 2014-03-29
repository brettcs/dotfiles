# .zshrc - third in the execution list; only for interactive shells.

### ENVIRONMENT VARIABLES
ZLS_COLORS="di=1;34"
WORDCHARS=$(echo $WORDCHARS | sed -e 's/\///')

# for ~/bin/reattach
export BCS_VAR_FILE="$HOME/.reattach"

### OPTIONS
# Autocompletion options
setopt AUTO_LIST
setopt AUTO_MENU
setopt ALWAYS_TO_END
setopt AUTO_PARAM_KEYS
setopt AUTO_PARAM_SLASH
setopt LIST_TYPES

# Miscellaneous options
setopt NO_BEEP
setopt CORRECT_ALL
setopt RM_STAR_SILENT
setopt TRANSIENT_RPROMPT
unsetopt MAIL_WARNING

### TERMINAL TITLE SETTING
if [ "$TERM" = "xterm" ] || [ -n "$BCS_SCREEN" ]; then
    titlebar="%m:%~"
fi

precmd() {
    [ -n "$titlebar" ] && echo -ne "\033]0;${(%)titlebar}\007"
    RPROMPT="%(1j!%B%F{cyan}%j%b%f !)"
    RPROMPT+="%(0?!!%B%F{red}%?%b%f )"
    local branch repocolor halfwidth
    halfwidth=$[${COLUMNS:-80} / 2]
    if [[ "$PWD" =~ "^$HOME/(w3)?repos/[^/]+" ]]; then
        if [ -d "$MATCH/.git" ]; then
	    repocolor='%F{green}'
            branch=$(git status 2>/dev/null | head -n 1 | cut -d' ' -f4-)
	elif [ -d "$MATCH/.hg" ]; then
	    repocolor='%F{cyan}'
	    branch=$(hg branch)
        fi
    fi
    if [ -n "$branch" ]; then
        RPROMPT+="(%B${repocolor}%$[$halfwidth - 20]>…>${branch}%>>%b${repocolor}:%f%1d"
    else
        local predir=$(print -P "%-1~/…")
        RPROMPT+="(%$[$halfwidth - 10]<${predir}<%~%<<"
    fi
    [ ${#dirstack} -ne 0 ] && \
        RPROMPT+="%B%F{yellow} +${#dirstack}"
    RPROMPT+="%b%f%k)"
}

PROMPT_SCREEN_HINT=$'%{\ek\e\\%}'
if [ "$LOGNAME" = "root" ]; then
    PROMPT_PCT="%B%F{red}%#%b%f"
elif [ -n "$BCS_SCREEN" ]; then
    PROMPT_PCT="%#"
else
    PROMPT_PCT="%B%F{yellow}%#%b%f"
fi

case "$(hostname -s)" in
    # Home machines
    locke) PROMPT_HOST_COLOR='%F{blue}' ;;
    isolde) PROMPT_HOST_COLOR='%F{cyan}' ;;
    stephan) PROMPT_HOST_COLOR='%K{magenta}%F{green}' ;;
    timulty) PROMPT_HOST_COLOR='%K{magenta}%F{cyan}' ;;
    # Personal servers
    llewellyn) PROMPT_HOST_COLOR='%F{yellow}' ;;
    panacea) PROMPT_HOST_COLOR='%K{magenta}%F{yellow}' ;;
    # Work machines
    brinstar) PROMPT_HOST_COLOR='%K{blue}%F{white}' ;;
    norfair) PROMPT_HOST_COLOR='%K{blue}%F{cyan}' ;;
    tourian) PROMPT_HOST_COLOR='%K{blue}%F{magenta}' ;;
    crateria) PROMPT_HOST_COLOR='%K{blue}%F{yellow}' ;;
    maridia) PROMPT_HOST_COLOR='%K{blue}%F{green}' ;;
    *) PROMPT_HOST_COLOR='%K{red}%F{white}' ;;
esac

_trysource() { [ -f "$1" ] && source "$1"; }
_trysource "$ZDOTDIR/.zshrc-local"
_trysource "$ZDOTDIR/.bcshrc"
[ -n "$BCS_SCREEN" ] && _trysource "$BCS_VAR_FILE"

trap "source \"$BCS_VAR_FILE\"" USR1

PROMPT="%B${PROMPT_HOST_COLOR}%m%b%f%k ${PROMPT_PCT} "
[ "$LOGNAME" != "brett" ] && PROMPT="%n@$PROMPT"
[ -n "$BCS_SCREEN" ] && PROMPT="${PROMPT_SCREEN_HINT}${PROMPT}"
true
