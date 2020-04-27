#!/bin/sh
scrot --overwrite /tmp/copycolor.png

eval $(xdotool getmouselocation --shell)
IMAGE=`convert /tmp/copycolor.png -depth 8 -crop 1x1+$X+$Y txt:-`
COLOR=`echo $IMAGE | grep -om1 '#\w\+'`
echo -n $COLOR | xclip -i -selection CLIPBOARD
notify-send "Color under mouse cursor: " $COLOR

