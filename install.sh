#!/bin/sh

set -e
set -u

cd "$(dirname "$0")"
umask 077

my_find() {
    find -mindepth 1 \
	\( -name .git -or -name .gitignore -or -name "$(basename "$0")" \) \
	-prune -or "$@" -print | sed -e 's/^\.\///'
}

ask_overwrite() {
    local fn=$1; shift
    local default_answer=${1:-N}
    case $default_answer in
        Y) local answers="(Y/n)" ;;
        *) local answers="(y/N)" ;;
    esac
    while true; do
	    echo
	    grep -viE '\bpass(word|wd|)\b' "$fn" | head -n 10
	    printf "OK to overwrite this $fn? $answers "
	    read ans
	    ans=$(echo "${ans:-$default_answer}" | tr '[:upper:]' '[:lower:]')
        case "$ans" in
            n|no) return 0 ;;
            y|yes) return 1 ;;
        esac
    done
}

can_overwrite() {
    local fn=$1; shift
    local target=$1; shift
    local overwrite_default=N
    test -e "$target" || return 0
    case "$fn" in
        # Files that start with standard settings
        .gitconfig|.hgrc|.mutt/muttrc)
            local linecount=$(wc -l <"$fn")
            head -n "$linecount" "$target" | cmp -s "$fn" - && return 1
            ;;
        *)
            cmp -s "$fn" "$target" && return 1
            overwrite_default=Y
            ;;
    esac
    ask_overwrite "$target" $overwrite_default
}

destdir=${1:-$HOME}

for dn in $(my_find -type d); do
    test -d "$destdir/$dn" || mkdir "$destdir/$dn"
done

for fn in $(my_find -type f); do
    target="$destdir/$fn"
    if can_overwrite "$fn" "$target"; then
        cp -b "$fn" "$target"
    fi
done

cd .bcsh
for fn in $(my_find -type f \! -name .bcsh\*); do
    source="$destdir/.bcsh/$fn"
    target="$destdir/$fn"
    if [ -h "$target" ] \
       && [ "$(readlink -e "$source")" = "$(readlink -e "$target")" ]; then
        continue
    elif can_overwrite "$source" "$target"; then
        ln -sb ".bcsh/$fn" "$target"
    fi
done
