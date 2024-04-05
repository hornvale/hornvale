#!/usr/bin/env bash

git fetch --prune;
for i in $(git branch | grep --invert-match '\*' | grep --invert-match 'main'); do
  git branch --delete --force "$i";
done;
