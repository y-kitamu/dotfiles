#!/bin/bash

set -eu

# git hub から fetch, push

cd /home/kitamura/dotfiles/
git checkout master
git fetch
git merge origin/master

git add .
git commit -m "cron commit ${date '+%Y%m%d'}"
