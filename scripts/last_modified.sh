#!/usr/bin/env bash
git show --quiet \
  --format=%ad \
  --date=format:"%F" \
  $(git log --format="%ad %H" \
      --date=format:%s \
      --grep="\[date skip\]" \
      --invert-grep $1 \
    | sort --reverse \
    | awk '{ print $2 }' \
    | head -n1
  )
