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
            n|no) return 1 ;;
            y|yes) return 0 ;;
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

link_if_ok() {
    local source="$1"; shift
    local target="$1"; shift
    if [ -h "$target" ] \
       && [ "$(readlink -e "$source")" = "$(readlink -e "$target")" ]; then
        true
    elif can_overwrite "$source" "$target"; then
        ln -srb "$source" "$target"
    fi
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

shelldir=".config/bcsh"
cd "$shelldir"
for fn in $(my_find -type f -name .\* ); do
    link_if_ok "$destdir/$shelldir/$fn" "$destdir/$fn"
done
for fn in .bash_profile .bashrc; do
    link_if_ok "$destdir/$shelldir/bashrc" "$destdir/$fn"
done
cd - >/dev/null

if [ ! -e "$destdir/$shelldir/screenrc" ]; then
    echo "screen 1 emacsclient --create-frame --alternate-editor=" \
         >>"$destdir/$shelldir/screenrc"
fi

if [ -z "${1:-}" ] && [ -d /run/systemd/system ] && { \
       [ -e "/var/lib/systemd/linger/$(id -nu)" ] \
       || loginctl enable-linger; } ; then
    cd .config/systemd/user
    systemctl --user daemon-reload
    grep -lFx '[Install]' *.service | while read service_name; do
        if [ "$(systemctl --user is-enabled "$service_name" || true)" = disabled ]; then
            systemctl --user enable "$service_name"
        fi
    done
    if screen_status="$(systemctl --user is-enabled screen.service)" \
            && [ "$screen_status" = enabled ]; then
        systemctl --user disable screen.service
        rm "$destdir/.config/systemd/user/screen.service"
    fi
    cd - >/dev/null
fi
