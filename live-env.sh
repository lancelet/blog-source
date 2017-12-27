#!/usr/bin/env bash

# First we'll do a clean of everything on first launch
stack build
stack exec blog clean
stack exec blog build

# Forked processes...

# Launch the site and watch
stack exec blog -- watch &
PID_SERVER="$!"

# Watch for changes in the blog generated files to trigger browser reload
fswatch -o -r _site | xargs -n1 -I {} \
  osascript -e 'tell application "Google Chrome" to tell the active tab of its first window to reload' &
PID_RELOAD="$!"

# Kill child processes on exit
trap "kill $PID_SERVER $PID_RELOAD" exit INT TERM
wait
