#!/bin/bash

# This file is for installing dotfiles in local machine.
# For installation, run below command;
# >> source install.sh

for f in .??*
do
    [[ "$f" == ".git" ]] && continue
	hf=../"$f"
	if [ -e "$hf" ]; then
		rm -r "$hf"
	fi
    ln -snfv ~/dotfiles/"$f" ~/
done


## emacs settings ##
# create backup directory
if [ ! -d ~/backups ]; then
    mkdir ~/backups
fi

    
