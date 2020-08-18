#!/bin/sh

tmux start-server

# create a session with five windows
tmux new-session -d -s dev -n mail -d "mutt"
tmux new-window -t dev:1 -n irc "weechat"
tmux new-window -t dev:2 -n files "ranger"
tmux new-window -t dev:3 -n server
tmux new-window -t dev:4

tmux attach -tdev

