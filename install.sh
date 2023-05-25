#!/bin/bash

# This file is for installing dotfiles in local machine.
# For installation, run below command;
# >> source install.sh


re='\.git.*'
for f in $(dirname "${BASH_SOURCE[0]}")/.??*
do
    [[ $f =~ $re ]] && continue
    basename=$(basename "$f")
    hf=$HOME/$basename
    if [ -e "$hf" ]; then
        rm -rf "$hf"
    fi

    ln -snfv ~/dotfiles/$basename ~/
done


# install vscode settings
ln -snfv ~/dotfiles/Code ~/.config/
