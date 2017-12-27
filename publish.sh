#!/usr/bin/env bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

GITDIR="$DIR/_gitblog"
REMOTE='git@github.com:lancelet/lancelet.github.io.git'
SITE="$DIR/_site"

# Build the site
pushd $DIR > /dev/null
stack clean
stack build
stack exec blog -- clean
stack exec blog -- build
popd > /dev/null

# Checkout the git repo
if [ ! -d "$GITDIR" ]; then
    git clone $REMOTE $GITDIR
fi

# Copy the site over to the git repo
rm -rf "$GITDIR/*"
cp -rf "$SITE/." "$GITDIR"

# Craft a commit message
TIMESTAMP=$(date "+%Y-%m-%dT%H:%M:%S%z")
COMMISH=$(git rev-parse --short HEAD)
COMMITMSG="Automatic publish: $COMMISH $TIMESTAMP"

# Publish
pushd $GITDIR > /dev/null
git add --all .
git commit -m "$COMMITMSG"
git push origin master
popd > /dev/null
