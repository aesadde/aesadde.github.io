#!/bin/sh

[[ $# -ne 1 ]] && usage echo -e "Specify commit message"

SITE=blog-aesadde

# Temporarily store uncommited changes
git stash

# Verify correct branch
git checkout develop

# Build new files
if stack build; then
  stack exec $SITE -- clean; stack exec $SITE -- build
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
  git stash apply
else
  echo "Site didn't build -- didn't publish"
fi
