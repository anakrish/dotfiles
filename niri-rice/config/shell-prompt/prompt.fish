# SPDX-License-Identifier: MIT

source (set -q XDG_CONFIG_HOME; and echo $XDG_CONFIG_HOME; or echo $HOME/.config)/shell-prompt/colors.fish

function fish_prompt
    set -l last_status $status
    set -l git_segment

    if git rev-parse --is-inside-work-tree >/dev/null 2>&1
        set -l branch (git branch --show-current 2>/dev/null)
        test -n "$branch"; or set branch (git rev-parse --short HEAD 2>/dev/null)

        set -l dirty
        if not git diff --quiet --ignore-submodules -- 2>/dev/null; or not git diff --cached --quiet --ignore-submodules -- 2>/dev/null
            set dirty '*'
        end

        set git_segment ' git:'$branch$dirty
    end

    set_color $RICE_CYAN_HEX
    printf '%s@%s' $USER (prompt_hostname)
    set_color normal
    printf ' '

    set_color $RICE_BLUE_HEX
    prompt_pwd
    set_color normal

    if test -n "$git_segment"
        set_color $RICE_MAGENTA_HEX
        printf '%s' $git_segment
        set_color normal
    end

    if test $last_status -ne 0
        set_color $RICE_ERROR_HEX
        printf ' exit:%s' $last_status
        set_color normal
    end

    printf '\n'
    set_color $RICE_GREEN_HEX
    printf '❯ '
    set_color normal
end
