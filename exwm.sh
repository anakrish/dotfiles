#!/bin/sh

xrandr --output eDP1 --scale 0.8x0.8
feh --bg-scale ~/Pictures/nordus.png
xmodmap ~/.Xmodmap
xkbset -bell -feedback sticky -twokey -latchlock
xkbset exp 64 '=sticky' '=twokey' '=latchlock'
xcompmgr &
#dunst &
# $HOME/.config/polybar/launch.sh
nm-applet &
#exec_always --not-startup-id redshift
blueman-applet &
sleep 1 && bluetooth off
#xautolock -detectsleep -time 10 -locker 'i3lock -c 000000 && sleep 1 && systemctl suspend' \
#	  -notify 30 -notifier 'notify-send -u normal  \"About to suspend in 30 seconds...\"' &

quake &
# emacs
emacs --daemon --with-profile optimal -eval "(my/init--exwm)"
exec emacsclient -c
