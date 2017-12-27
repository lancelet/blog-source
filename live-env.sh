#!/usr/bin/env bash

# Function to reload the browser...
function browser_reload() {
    osascript -e 'tell application "Google Chrome" to tell the active tab of its first window to reload'
}

# First we'll do a clean of everything on first launch
stack build
stack exec blog clean
stack exec blog build

# Forked processes...

# Watch for changes in haskell files to trigger stack rebuilds
fswatch -e '.*' -i '[.]hs$' -o -r . | xargs -n1 -I {} stack exec blog rebuild &
PID_STACK="$!"

# Watch for changes in the blog source to trigger blog rebuilds
fswatch -e '.*'       \
        -i '[.]html$' \
        -i '[.]md$'   \
        -i '[.]css$'  \
        -e '_site/.*' \
        -o -r . | xargs -n1 -I {} stack exec blog build &
PID_BLOG="$!"

# Watch for changes in the blog generated files to trigger browser reload
fswatch -o -r _site | xargs -n1 -I {} browser_reload
PID_RELOAD="$!"

# Kill child processes on exit
trap "kill $PID_STACK $PID_BLOG $PID_RELOAD" exit INT TERM
wait
