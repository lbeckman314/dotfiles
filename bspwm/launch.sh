#!/bin/bash

# https://www.reddit.com/r/bspwm/comments/2mpui3/attempting_to_autostart_a_few_programs/
# wait for an internet connection
# while ! wget http://google.com -O- 2>/dev/null | grep -q Lucky; do
#   sleep .1
# done

# launch stuff
xfce4-terminal &
firefox &
emacs &
evince &
