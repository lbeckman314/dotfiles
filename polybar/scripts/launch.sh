#!/usr/bin/env bash
# https://github.com/gugahoi/dotfiles-linux/blob/master/config/polybar/scripts/launch.sh
# https://www.reddit.com/r/i3wm/comments/7t0ekg/polybar_issues_in_i3/

name=top
pkill polybar
if type "xrandr"; then
  for monitor in $(xrandr --query | grep " connected" | cut -d" " -f1); do
    MONITOR=${monitor} polybar --reload ${name}&
  done
else
  polybar --reload ${name} &
fi
