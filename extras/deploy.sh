#!/bin/sh

[[ $# -ne 1 ]] && echo -e "Specify commit message"

ROOT=/Users/aesadde/Projects/aesadde-web/
SITE=web
CONSOLIDATION=/Users/aesadde/Documents/Oxford/Dissertation/Consolidation/page/

# Temporarily store uncommited changes
git stash

# Verify correct branch
git checkout develop

# Build new files
if stack build; then

  # check that the dissertation site is up to date
  # (this might not be the smartest thing but works for now!)
  cd $CONSOLIDATION
  stack exec page -- deploy
  cd $ROOT

  stack exec $SITE -- clean; stack exec $SITE -- build
  # Get previous files
  git checkout -b master --track origin/master

  # Overwrite existing files with new files
  cp -a _site/* .

  # Commit
  git add -A
  git commit -m "Publish $1"

  # Push
  git push origin master:master

  # Restoration
  git checkout develop
  git branch -D master
  git push source develop
  git stash apply
else
  echo "Site didn't build -- didn't publish"
fi
