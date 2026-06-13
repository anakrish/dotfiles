#!/usr/bin/env bash
# SPDX-License-Identifier: MIT

if [[ $- != *i* ]]; then
    return
fi

source "${XDG_CONFIG_HOME:-$HOME/.config}/shell-prompt/colors.sh"

__rice_fg() {
    printf '\001\033[38;2;%sm\002' "$1"
}

__rice_bg() {
    printf '\001\033[48;2;%sm\002' "$1"
}

__rice_reset=$'\001\033[0m\002'
__rice_bold=$'\001\033[1m\002'

__rice_git_segment() {
    git rev-parse --is-inside-work-tree >/dev/null 2>&1 || return

    local branch dirty
    branch="$(git branch --show-current 2>/dev/null)"
    if [ -z "$branch" ]; then
        branch="$(git rev-parse --short HEAD 2>/dev/null)"
    fi

    if ! git diff --quiet --ignore-submodules -- 2>/dev/null ||
       ! git diff --cached --quiet --ignore-submodules -- 2>/dev/null; then
        dirty="*"
    fi

    printf ' %s%s%s%s' \
        "$(__rice_fg "$RICE_MAGENTA_RGB")" \
        "git:$branch$dirty" \
        "$__rice_reset" \
        ""
}

__rice_prompt_command() {
    local status=$?
    local status_segment=""

    if [ "$status" -ne 0 ]; then
        status_segment=" $(__rice_fg "$RICE_ERROR_RGB")exit:$status$__rice_reset"
    fi

    PS1="\[\e]0;\w\a\]$(__rice_fg "$RICE_CYAN_RGB")$__rice_bold\u@\h$__rice_reset $(__rice_fg "$RICE_BLUE_RGB")\w$__rice_reset$(__rice_git_segment)$status_segment\n$(__rice_fg "$RICE_GREEN_RGB")❯$__rice_reset "
}

PROMPT_COMMAND="__rice_prompt_command"
