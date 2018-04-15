# greyline - Powerline theme based on bira and agnoster's themes

## TODO Respect colour capability set in TERM

__greyline_prompt () {
    local ret_val=$?

    local LC_ALL=""
    local LC_CTYPE="en_US.UTF-8"

    local separator=$'\ue0b0'          # 
    local git_symbol=$'\ue0a0'         # 

    local black=0
    local red=1
    local green=2
    local yellow=3
    local blue=4
    local magenta=5
    local cyan=6
    local white=7
    local grey=8

    # Background colours
    local dark=$black
    local light=$grey

    # Line setup booleans
    local is_dark_segment=0
    local is_start_segment=0

    #############################################
    ## Seting prompt colours

    # call with the number of the color you want 256 available or empty to reset to default
    foreground () {
        if [[ -n $1 ]]; then
            echo -n "%{\033[38;5;${1}m%}"
        else
            # reset foreground
            echo -n "%{\033[39m%}"
        fi
    }

    background () {
        echo -n "%{\033[48;5;${1:-0}m%}"
    }

    #############################################
    ## Line setup

    # Begin a line with a dark segment
    start_dark () {
        is_dark_segment=1
        is_start_segment=1
    }

    # Begin a line with a light segment
    start_light () {
        is_dark_segment=0
        is_start_segment=1
    }

    # End the current line and reset the background. This does not print a new line
    end_line () {
        # reset background colour
        echo -n "%{\033[49;m%}"
        if ! ((is_start_segment)); then
            if ((is_dark_segment)); then
                foreground $light
            else
                foreground $dark
            fi
            echo -n $separator
        fi
        foreground
    }

    # produces a segment with alternating light and dark properties
    segment () {
        [[ -n $1 ]] || return

        if ((is_dark_segment)); then

            foreground $light
            background $dark
        else
            foreground $dark
            background $light
        fi

        if ((is_start_segment)); then
            is_start_segment=$((1-is_start_segment))
        else
            echo -n $separator
        fi

        foreground $2
        is_dark_segment=$((1-is_dark_segment))

        echo -n " $1 "
    }

    #############################################
    ## Line builders - functions that produce zero or more prompt segments

    user_host () {
        # user@host
        if [[ $UID -eq 0 ]]; then
            segment "%n@%m" $red
        else
            segment "%n@%m" $green
        fi
    }

    git_status () {
        (( $+commands[git] )) || return

        local inf=$(git_prompt_info)

        [[ -n $inf ]] || return

        local colour=$white
        [[ -n $(parse_git_dirty) ]] && colour=$yellow

        segment "$git_symbol $inf" $colour
    }

    current_dir () {
        segment "%~" $blue
    }

    prompt_char () {
        segment "$"
    }

    return_status () {
        [[ $ret_val -ne 0 ]] && segment "%? ↵" $red
    }

    #############################################
    ## Print the line

    echo -n "$(# Line 1
               start_light;
               user_host;
               current_dir;
               git_status;
               return_status;
               end_line)
$(# Line 2
               start_dark;
               prompt_char;
               end_line) "

}

ZSH_THEME_GIT_PROMPT_PREFIX=''
ZSH_THEME_GIT_PROMPT_SUFFIX=''

PROMPT='$(__greyline_prompt)'
RPS1=''
