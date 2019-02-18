
# enable .xkb keymap
if [ -s $HOME/dotfiles/.xkb/keymap/myxkb ]
then
    sleep 1
    xkbcomp -I$HOME/dotfiles/.xkb ~/dotfiles/.xkb/keymap/myxkb $DISPLAY
fi

