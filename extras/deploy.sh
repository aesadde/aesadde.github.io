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

  cd $CONSOLIDATION
  stack exec page -- deploy
  cd $ROOT

  rsync --checksum -ave \
    /Users/aesadde/Documents/Docs/Curriculum/current/cv.pdf $ROOT/files/resume.pdf

  stack exec $SITE -- clean; stack exec $SITE -- build

  # Overwrite existing files with new files
  rsync --checksum -ave _site/* deploy/
  cd deploy

  # Commit
  git add -A
  git commit -m "Publish $1"

  # Push
  git push origin master

  # Restoration
  cd ..
  git stash apply
else
  echo "Site didn't build -- didn't publish"
fi
