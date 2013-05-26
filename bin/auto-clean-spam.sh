#!/bin/sh

set -e
set -u

cd "$HOME/Maildir/.Spam"

find cur new -type f -mtime +7 | while read msgfile; do
    if formail -x X-Spam-Level <"$msgfile" | grep -qE '\*{20,}'; then
	rm "$msgfile"
    fi
done
