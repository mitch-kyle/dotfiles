#!/bin/sh

[[ -f ~/.xprofile ]] && . ~/.xprofile

export VISUAL="emacsclient"
export EDITOR="$VISUAL"

export X_BACKGROUND="${X_BACKGROUND:-/usr/share/backgrounds/background.jpg}"

pgrep redshift-gtk &> /dev/null || redshift-gtk &
pgrep oneko &> /dev/null || oneko -tora -fg '#f8f8f2' -bg '#272822' &
pgrep xcompmgr &> /dev/null || xcompmgr &

if [ -f "$X_BACKGROUND" ]; then
    feh --bg-scale "$X_BACKGROUND"
fi

exec ${@:-${DEFAULT_X_SESSION:-xterm}}
