#!/usr/bin/env bash

for i in $(git branch | grep -v '\*' | grep -v 'main'); do git branch -d "$i"; done;
