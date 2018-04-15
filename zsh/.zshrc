#!/usr/bin/zsh

# Zsh Options
HISTFILE=~/.histfile
HISTSIZE=5000
SAVEHIST=10000
setopt appendhistory beep extendedglob nomatch
bindkey -e
zstyle :compinstall filename '/home/mitch/.zshrc'
autoload -Uz compinit
compinit

# Oh My Zsh
export OH_MY_ZSH_HOME="${OH_MY_ZSH_HOME:-/usr/share/oh-my-zsh}"

if [ -f "$OH_MY_ZSH_HOME/oh-my-zsh.sh" ]; then

    if [ -f "$OH_MY_ZSH_HOME/zshrc" ]; then
        . "$OH_MY_ZSH_HOME/zshrc"
    fi

    plugins=(
        git
        docker
        tmux
        lein
        jsontools
        colorize
        emoji
    )

    ZSH_THEME=""
    ZSH_TMUX_FIXTERM=true
    . "$OH_MY_ZSH_HOME/oh-my-zsh.sh"

    #cat but with syntax highlighting
    cless () {
        less <<< "$(colorize_via_pygmentize $@)"
    }

    alias c=colorize_via_pygmentize
    alias cl=cless

    . ~/.config/zsh/greyline.zsh-theme
fi

# Aliases
alias e='$EDITOR'
alias ls='ls --color=auto'
alias ll='ls -l'
alias la='ls -a'

alias sstart='sudo systemctl start'
alias sstop='sudo systemctl stop'
alias srestart='sudo systemctl restart'
alias sstatus='sudo systemctl status'
alias senable='sudo systemctl enable'
alias sdisable='sudo systemctl disable'

alias ustart='systemctl --user start'
alias ustop='systemctl --user stop'
alias urestart='systemctl --user restart'
alias ustatus='systemctl --user status'
alias uenable='systemctl --user enable'
alias udisable='systemctl --user disable'

alias music='ncmpcpp'
