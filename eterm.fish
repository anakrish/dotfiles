function man
  emacsclient -q -u -e "(man \"$argv\")"
end

function make
  emacsclient -q -u -e "(compile \"make $argv\")"
end

function cmake
  emacsclient -q -u -e "(compile \"cmake $argv\")"
end

function ctest
  emacsclient -q -u -e "(compile \"ctest $argv\")"
end

function ninja
  emacsclient -q -u -e "(compile \"ninja $argv\")"
end

function async
  emacsclient -q -u -e "(async-shell-command \"$argv\")"
end

function ff
  emacsclient "$argv"
end

functions -c fish_prompt eterm_old_fish_prompt

function fish_prompt --description 'Write out the prompt; do not replace this. Instead, put this at end of your file.'
  set -l pwd (pwd)
  set -l cmd "(with-current-buffer (window-buffer (selected-window)) (cd \"$pwd\"))"
  emacsclient -u -q -e $cmd
  printf "%b" (string join "\n" (eterm_old_fish_prompt))
end
