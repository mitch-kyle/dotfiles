#!/bin/bash

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

session=${OVERRIDE_X_SESSION:-${@:-${DEFAULT_X_SESSION:-xterm}}}

if [[ $session =~ emacs ]]; then
    export _JAVA_AWT_WM_NONREPARENTING=1
fi

exec $session
