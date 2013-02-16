# .zshrc - third in the execution list; only for interactive shells.

### ENVIRONMENT VARIABLES
PROMPT_BOLD=$'%{\e[1m%}'
PROMPT_NORMAL=$'%{\e[22m%}'
PROMPT_RESET=$'%{\e[0m%}'

PROMPT_RED=$'%{\e[1;31m%}'
PROMPT_GREEN=$'%{\e[1;32m%}'
PROMPT_YELLOW=$'%{\e[1;33m%}'
PROMPT_BLUE=$'%{\e[1;34m%}'
PROMPT_PURPLE=$'%{\e[1;35m%}'
PROMPT_CYAN=$'%{\e[1;36m%}'

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
    RPROMPT="%(1j!${PROMPT_CYAN}%j${PROMPT_RESET} !)"
    RPROMPT+="%(0?!!${PROMPT_RED}%?${PROMPT_RESET} )"
    local branch repocolor halfwidth
    halfwidth=$[${COLUMNS:-80} / 2]
# PROMPT_GREEN=$'%{\e[1;32m%}'
    if [[ "$PWD" =~ "^$HOME/(w3)?repos/[^/]+" ]]; then
        if [ -d "$MATCH/.git" ]; then
	    repocolor=$'%{\e[32m%}'  # green
            branch=$(git status 2>/dev/null | head -n 1 | cut -d' ' -f4-)
	elif [ -d "$MATCH/.hg" ]; then
	    repocolor=$'%{\e[36m%}'  # cyan
	    branch=$(hg branch)
        fi
    fi
    if [ -n "$branch" ]; then
        RPROMPT+="(${PROMPT_BOLD}${repocolor}%$[$halfwidth - 20]>…>${branch}%>>${PROMPT_NORMAL}:${PROMPT_RESET}%1d"
    else
        local predir=$(print -P "%-1~/…")
        RPROMPT+="(%$[$halfwidth - 10]<${predir}<%~%<<"
    fi
    [ ${#dirstack} -ne 0 ] && \
        RPROMPT+="${PROMPT_YELLOW} +${#dirstack}"
    RPROMPT+="${PROMPT_RESET})"
}

PROMPT_SCREEN_HINT=$'%{\ek\e\\%}'
if [ "$LOGNAME" = "root" ]; then
    PROMPT_PCT="${PROMPT_RED}%#${PROMPT_RESET}"
elif [ -n "$BCS_SCREEN" ]; then
    PROMPT_PCT="%#"
else
    PROMPT_PCT="${PROMPT_YELLOW}%#${PROMPT_RESET}"
fi

# 0=black, 1=red, 2=green, 3=yellow, 4=blue, 5=purple, 6=cyan, 7=white
case "$(hostname -s)" in
    # Home machines
    isolde) PROMPT_HOST_COLOR=$'%{\e[1;36m%}' ;;
    stephan) PROMPT_HOST_COLOR=$'%{\e[1;45;32m%}' ;;
    timulty) PROMPT_HOST_COLOR=$'%{\e[1;45;36m%}' ;;
    ozymandias) PROMPT_HOST_COLOR=$'%{\e[1;30m%}' ;;
    # Personal servers
    llewellyn) PROMPT_HOST_COLOR=$'%{\e[1;33m%}' ;;
    panacea) PROMPT_HOST_COLOR=$'%{\e[1;45;33m%}' ;;
    fencepost) PROMPT_HOST_COLOR=$'%{\e[1;45;35m%}' ;;
    # Work machines
    serenity) PROMPT_HOST_COLOR=$'%{\e[1;34m%}' ;;
    copyrighteous) PROMPT_HOST_COLOR=$'%{\e[1;44;37m%}' ;;
    *) PROMPT_HOST_COLOR=$'%{\e[1;41;31m%}' ;;
esac

_trysource() { [ -f "$1" ] && source "$1"; }
_trysource "$HOME/.zshrc-local"
_trysource "$HOME/.bcshrc"
[ -n "$BCS_SCREEN" ] && _trysource "$BCS_VAR_FILE"

trap "source \"$BCS_VAR_FILE\"" USR1

PROMPT="${PROMPT_HOST_COLOR}%m${PROMPT_RESET} ${PROMPT_PCT} "
[ "$LOGNAME" != "brett" ] && PROMPT="%n@$PROMPT"
[ -n "$BCS_SCREEN" ] && PROMPT="${PROMPT_SCREEN_HINT}${PROMPT}"
true
