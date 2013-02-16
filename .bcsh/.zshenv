# .zshenv - first in the execution list; for all shells

### ENVIRONMENT VARIABLES
SAVEHIST=500
HISTFILE="$HOME/.zsh_history"
ZDOTDIR="$HOME/.bcsh"

export SAVEHIST HISTFILE ZDOTDIR

### OPTIONS
setopt CLOBBER
setopt APPEND_HISTORY
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_IGNORE_DUPS
setopt HIST_FIND_NO_DUPS
setopt HIST_SAVE_NO_DUPS

source "$ZDOTDIR/.bcshenv"
