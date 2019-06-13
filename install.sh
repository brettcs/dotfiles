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

should_enable() {
    local unit_name="$1"; shift
    local unit_status
    test -e /run/systemd/system
    if unit_status="$(systemctl --user is-enabled "$unit_name")" \
            || [ "$unit_status" != disabled ]; then
        return 1
    fi
    case "$unit_name" in
        *.service) test -e "/var/lib/systemd/linger/$(id -nu)" ;;
        *) return 0 ;;
    esac
}

destdir=${1:-$HOME}

if [ -e /run/systemd/system ] \
       && screen_status="$(systemctl --user is-enabled screen.service)" \
       && [ "$screen_status" = enabled ]; then
    systemctl --user disable screen.service
    rm "$destdir/.config/systemd/user/screen.service"
fi

for dn in $(my_find -type d); do
    test -d "$destdir/$dn" || mkdir "$destdir/$dn"
done

for fn in $(my_find -type f); do
    target="$destdir/$fn"
    if can_overwrite "$fn" "$target"; then
        cp -b "$fn" "$target"
        fn_dir=$(dirname "$fn")
        fn_base=$(basename "$fn")
        if [ "$fn_dir" = .config/systemd/user ] && should_enable "$fn_base"; then
            systemctl --user enable "$fn_base"
        fi
    fi
done

shelldir=".config/bcsh"
cd "$shelldir"
for fn in $(my_find -type f -name .\* ); do
    source="$destdir/$shelldir/$fn"
    target="$destdir/$fn"
    if [ -h "$target" ] \
       && [ "$(readlink -e "$source")" = "$(readlink -e "$target")" ]; then
        continue
    elif can_overwrite "$source" "$target"; then
        ln -sb "$shelldir/$fn" "$target"
    fi
done

if [ ! -e "$HOME/$shelldir/screenrc" ]; then
    echo "screen 1 emacsclient --create-frame --alternate-editor=emacs" \
         >>"$HOME/$shelldir/screenrc"
fi
