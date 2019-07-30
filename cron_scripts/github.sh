#!/bin/bash

set -eu

set USERNAME "y-kitamu"
set PASSWORD "ymyk6422"

# git hub から fetch, push

cd /home/kitamura/dotfiles/
git checkout master
git fetch
git merge origin/master

git add .
git commit -m "cron commit `date '+%Y%m%d'`"

set timeout 5
spawn git push -u origin master
expect {
    "Username for 'https://github.com':" {
        send "${USERNAME}\n"
        exp_continue
    }
    "Password for 'https://${}@github.com':" {
        send "${PASSWORD}\n"
    }
}
expect {
    "\\\$" {
        exit 0
    }
}
