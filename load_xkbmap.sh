#!/bin/sh

# enable .xkb keymap
if [ -s $HOME/.xkb/keymap/myxkb ]
then
    sleep 1
    xkbcomp -I$HOME/.xkb $HOME/.xkb/keymap/myxkb $DISPLAY 2>/dev/null
fi

