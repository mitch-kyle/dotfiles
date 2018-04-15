# greyline - Powerline theme based on bira and agnoster's themes

greyline_separator=$'\ue0b0'
greyline_is_dark_segment=0

# Named colors for readability
greyline_black=0
greyline_red=1
greyline_green=2
greyline_yellow=3
greyline_blue=4
greyline_magenta=5
greyline_cyan=6
greyline_white=7
greyline_grey=8

greyline_dark=$greyline_black
greyline_light=$greyline_grey

# call with the number of the color you want 256 available or empty to reset to default
greyline_foreground () {
    if [[ -n "$1" ]]; then
        echo -n "\033[38;5;${1}m"
    else
        echo -n "\033[39m"
    fi
}

greyline_background () {
    if [[ -z $1 ]]; then
        greyline_background 0
    else
        echo -n "\033[48;5;${1}m"
    fi
}

greyline_end_line () {
    echo -n "\033[49;m"
    if ((greyline_is_dark_segment)); then
        greyline_foreground $greyline_dark
    else
        greyline_foreground $greyline_light
    fi
    echo -n $greyline_separator
    greyline_foreground
}

greyline_segment() {
    greyline_foreground $greyline_previous_bg

    if ((greyline_is_dark_segment)); then
        greyline_foreground $greyline_dark
        greyline_background $greyline_light
    else
        greyline_foreground $greyline_light
        greyline_background $greyline_dark
    fi

    echo -n $greyline_separator

    greyline_foreground $2
    greyline_is_dark_segment=$((1-greyline_is_dark_segment))

    [[ -n $1 ]] && echo -n " $1 "
}

greyline_start_segment_dark () {
    greyline_is_dark_segment=1
    greyline_foreground $2
    greyline_background $greyline_dark
    echo -n " $1 "
}

greyline_start_segment_light () {
    greyline_is_dark_segment=0
    greyline_foreground $2
    greyline_background $greyline_light
    echo -n " $1 "
}

greyline_user_host() {
    if [[ $UID -eq 0 ]]; then
        $1 "%n@%m" $greyline_red
    else
        $1 "%n@%m"
    fi
}

greyline_git_status() {
  (( $+commands[git] )) || return
  local PL_BRANCH_CHAR
  () {
    local LC_ALL="" LC_CTYPE="en_US.UTF-8"
    PL_BRANCH_CHAR=$'\ue0a0'         # 
  }
  local ref dirty mode repo_path colour
  repo_path=$(git rev-parse --git-dir 2>/dev/null)

  if $(git rev-parse --is-inside-work-tree >/dev/null 2>&1); then
    dirty=$(parse_git_dirty)
    if [[ -n $dirty ]]; then
      colour="$greyline_yellow"
    else
      colour="$greyline_green"
    fi

    ref=$(git symbolic-ref HEAD 2> /dev/null) || ref="➦ $(git rev-parse --short HEAD 2> /dev/null)"
    if [[ -e "${repo_path}/BISECT_LOG" ]]; then
      mode=" <B>"
    elif [[ -e "${repo_path}/MERGE_HEAD" ]]; then
      mode=" >M<"
    elif [[ -e "${repo_path}/rebase" || -e "${repo_path}/rebase-apply" || \
                -e "${repo_path}/rebase-merge" || -e "${repo_path}/../.dotest" ]]; then
      mode=" >R>"
    fi

    setopt promptsubst
    autoload -Uz vcs_info

    zstyle ':vcs_info:*' enable git
    zstyle ':vcs_info:*' get-revision true
    zstyle ':vcs_info:*' check-for-changes true
    zstyle ':vcs_info:*' stagedstr '✚'
    zstyle ':vcs_info:*' unstagedstr '●'
    zstyle ':vcs_info:*' formats ' %u%c'
    zstyle ':vcs_info:*' actionformats ' %u%c'
    vcs_info

    $1 "${ref/refs\/heads\//$PL_BRANCH_CHAR}${dirty}${vcs_info_msg_0_%% }${mode}" $colour
  fi
}

greyline_current_dir() {
    $1 "%~" $greyline_blue
}

greyline_prompt() {
    $1 "$"
}

greyline_return_status () {
    $1 "%(?..%{$fg[red]%} %? ↵%{$reset_color%})" "%(?..)"
}

greyline_gen_prompt() {
    greyline_user_host     greyline_start_segment_light
    greyline_current_dir   greyline_segment
    greyline_git_status    greyline_segment
    greyline_end_line
    greyline_return_status echo

    greyline_prompt        greyline_start_segment_dark
    greyline_end_line
}

PROMPT='$(greyline_gen_prompt) '
RPS1=""
