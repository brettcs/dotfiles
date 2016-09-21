# ~/.bashrc: executed by bash(1) for non-login shells.

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

export ZDOTDIR="${ZDOTDIR:-$HOME/.bcsh}"

source "$ZDOTDIR/.bcshrc"
