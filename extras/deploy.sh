#!/bin/sh

[[ $# -ne 1 ]] && usage echo -e "Specify commit message"

BLOG=blog-aesadde

# Temporarily store uncommited changes
git stash

# Verify correct branch
git checkout develop

# Build new files
stack exec $BLOG clean
stack exec $BLOG build

# Get previous files
git checkout -b master --track origin/master

# Overwrite existing files with new files
cp -a _site/. .

# Commit
git add -A
git commit -m "Publish $1"

# Push
git push origin master:master

# Restoration
git checkout develop
git branch -D master
git stash pop
