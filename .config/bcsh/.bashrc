# ~/.bashrc: executed by bash(1) for non-login shells.

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

case "$(id -nu)" in
    brett) PS1='\[\e[1;37m\]\h\[\e[0;37m\] \w\[\e[0m\] \$ ' ;;
    *) PS1='\u@\[\e[1;37m\]\h\[\e[0;37m\] \w\[\e[0m\] \$ ' ;;
esac

export ZDOTDIR="${ZDOTDIR:-$HOME/.config/bcsh}"

source "$ZDOTDIR/rc"
