# .zshenv - first in the execution list; for all shells

### ENVIRONMENT VARIABLES
export ZDOTDIR="$HOME/.bcsh"

export HISTFILE="$ZDOTDIR/.zsh_history"
export HISTSIZE=300
export SAVEHIST=250

### OPTIONS
setopt CLOBBER
setopt APPEND_HISTORY
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_IGNORE_DUPS
setopt HIST_FIND_NO_DUPS
setopt HIST_SAVE_NO_DUPS
setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY

source "$ZDOTDIR/.bcshenv"
