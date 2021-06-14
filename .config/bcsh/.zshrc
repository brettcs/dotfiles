# .zshrc - third in the execution list; only for interactive shells.

# Autocompletion options
setopt AUTO_LIST
setopt AUTO_MENU
setopt ALWAYS_TO_END
setopt AUTO_PARAM_KEYS
setopt AUTO_PARAM_SLASH
unsetopt AUTO_REMOVE_SLASH
setopt LIST_TYPES

# History options
HISTFILE="${XDG_STATE_HOME:-$HOME/.local/state}/zsh/history"
HISTSIZE=1000
SAVEHIST=1000
setopt APPEND_HISTORY
setopt CLOBBER
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_IGNORE_DUPS
setopt HIST_FIND_NO_DUPS
setopt HIST_SAVE_NO_DUPS
setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY
mkdir -p "$(dirname "$HISTFILE")"

# Miscellaneous options
setopt CORRECT
setopt NO_BEEP
setopt RM_STAR_SILENT
setopt TRANSIENT_RPROMPT
unsetopt MAIL_WARNING

WORDCHARS=$(echo $WORDCHARS | sed -e 's/\///')

autoload -U compinit
autoload zsh/complist
compinit
zstyle ':completion:*' list-colors 'di=1;34'
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}'

autoload -Uz vcs_info
zstyle ':vcs_info:*' enable bzr cvs git hg svn
zstyle ':vcs_info:*' formats '%F{cyan}%r:%B%b%%b%F{cyan}:%f'
zstyle ':vcs_info:*' actionformats \
    '%F{cyan}%r:%B%F{yellow}%a%f%%b on %F{cyan}%B%b%%b%F{cyan}:%f'
zstyle ':vcs_info:git:*' formats '%F{green}%r:%B%b%%b%F{green}:%f'
zstyle ':vcs_info:git:*' actionformats \
    '%F{green}%r:%B%F{yellow}%a%f%%b on %F{green}%B%b%%b%F{green}:%f'

### TERMINAL TITLE SETTING
if [ "$TERM" = "xterm" ] || [ -n "$BCS_SCREEN" ]; then
    titlebar="%m:%~"
fi

precmd() {
    [ -n "$titlebar" ] && echo -ne "\033]0;${(%)titlebar}\007"
    vcs_info
    RPROMPT="%(1j!%B%F{cyan}%j%b%f !)"
    RPROMPT+="%(0?!!%B%F{red}%?%b%f )"
    local halfwidth=$[${COLUMNS:-80} / 2]
    if [ -n "$vcs_info_msg_0_" ]; then
        RPROMPT+="(${vcs_info_msg_0_}%1d"
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

case "$(hostname --fqdn)" in
    larnblatt.*) PROMPT_HOST_COLOR='%F{magenta}' ;;
    mililani.*) PROMPT_HOST_COLOR='%F{yellow}' ;;
    nuc10g|nuc10g.*) PROMPT_HOST_COLOR='%F{cyan}' ;;
    xps9310|xps9310.*) PROMPT_HOST_COLOR='%F{green}' ;;
    *.brettcsmith.org) PROMPT_HOST_COLOR='%F{white}' ;;
    *) PROMPT_HOST_COLOR='%F{red}' ;;
esac

_trysource() { [ -f "$1" ] && source "$1"; }
_trysource "$ZDOTDIR/local.zsh"
_trysource "$ZDOTDIR/rc"

PROMPT="%B${PROMPT_HOST_COLOR}%m%b%f%k ${PROMPT_PCT} "
[ -n "$BCS_SCREEN" ] && PROMPT="${PROMPT_SCREEN_HINT}${PROMPT}"
true
