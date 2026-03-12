#!/usr/bin/env zsh

# Split into 6 panes (2 rows x 3 cols)
tmux split-window -h
tmux split-window -h
tmux select-pane -t 1
tmux split-window -v
tmux select-pane -t 3
tmux split-window -v
tmux select-pane -t 5
tmux split-window -v

# Start servers in panes 0-4
servers=(Bailey Bona Campbell Clark Jaquez)
for i in {1..5}; do
    tmux send-keys -t $i "tail -f logs/${servers[$((i))]}.log" Enter
done
