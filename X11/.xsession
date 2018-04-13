#!/bin/sh

export VISUAL="emacsclient"
export EDITOR="$VISUAL"

export X_BACKGROUND="${X_BACKGROUND:-/usr/share/backgrounds/background.jpg}"

amixer set Mic mute

supplimentary () {
    pgrep xcompmgr || xcompmgr &

    if [ -f "$X_BACKGROUND" ]; then
        feh --bg-scale "$X_BACKGROUND"
    fi
}

default () {
    supplimentary
    exec emacs --funcall mexwm-enable
}

case "$1" in
    awesome)
        supplimentary
        exec awesome
	;;
    "")
        default
        ;;
    /etc/X11/Xsession)
	default
        ;;
    *)
        exec $@
        ;;
esac
