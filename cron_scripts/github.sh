#!/bin/bash

#set -eu

. ./git_account_name.sh 

# git hub から fetch, push

cd /home/kitamura/dotfiles/
git checkout master
git fetch
git merge origin/master

git add .
git commit -m "cron commit `date '+%Y%m%d'`"

expect -c "
       set timeout 5
       spawn git push -u origin master
       expect \"Username for 'https://github.com':\"
       send \"${USERNAME}\n\"
       expect \"Password for 'https://${USERNAME}@github.com':\"
       send \"${PASSWORD}\n\"
       expect \"\\\$\" 
       exit 0
"
