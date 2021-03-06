#!/bin/bash
# git hub から fetch, push する
# 引数に対象ディレクトリを指定することができる
# source ~/dotfiles/cron_scripts/github.sh /path/to/repository

set -eu

git config --local user.name "kitamura"
git config --local user.email "ymyk6602@gmail.com" 

cd /home/kitamura/dotfiles
. ./cron_scripts/git_account_name.sh 

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
