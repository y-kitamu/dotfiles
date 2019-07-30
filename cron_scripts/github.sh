#!/bin/bash

set -eu

cd /home/kitamura/dotfiles
. ./cron_scripts/git_account_name.sh 

# git hub から fetch, push

if [ $# == 1 ]; then
    cd ${1}
fi

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
