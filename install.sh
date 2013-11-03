#!/bin/sh

set -e
set -u

cd "$(dirname "$0")"
umask 077

my_find() {
    find -mindepth 1 \
	\( -name .git -or -name .gitignore -or -name "$(basename "$0")" \) \
	-prune -or "$@" -print
}

ask_overwrite() {
    local fn=$1; shift
    while true; do
	echo
	head -n 10 "$fn"
	printf "OK to overwrite this $fn? (y/N) "
	read ans
	ans=$(echo "${ans:-n}" | tr '[:upper:]' '[:lower:]')
	if [ "$ans" = n ] || [ "$ans" = no ]; then
	    return 1
	elif [ "$ans" = y ] || [ "$ans" = yes ]; then
	    return 0
	fi
    done
}

can_overwrite() {
    local fn=$1; shift
    local target=$1; shift
    test -e "$target" || return 0
    cmp -s "$fn" "$target" && return 1
    ask_overwrite "$target"
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
