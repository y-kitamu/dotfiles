# ~/.bash_aliases: list of environment variables & aliases. read in ~/.bashrc.

### environment variables ###
# use emacs as default editor
export EDITOR="emacs"

# anaconda setting
# if [ -d $HOME/.pyenv ]; then
#     export PYENV_ROOT="$HOME/.pyenv"
#     export PATH="$PYENV_ROOT/bin:$PATH"
#     eval "$(pyenv init -)"
#     export PATH="$PYENV_ROOT/versions/anaconda3-5.3.0/bin/:$PATH"
# fi

# setting for installing package in local environment
# set PATH so it includes user's private directory if it exists
# if [ -d "$HOME/local" ] ; then
#     export PATH="$HOME/local:$PATH"
#     if [ -d "$HOME/local/bin" ] ; then
#        export PATH="$HOME/bin:$PATH"
#     fi
#     if [ -d "$HOME/local/include" ] ; then
#        export CPATH="$HOME/local/include"
#     fi  
# fi

# for gsettings error
export GIO_EXTRA_MODULES=/usr/lib/x86_64-linux-gnu/gio/modules


### aliases ###
# python & anaconda aliases
# alias activate='source /home/kitamura/.pyenv/versions/anaconda3-4.1.0/bin/activate'
# alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'
# #alias jupyter='cd ~/Dropbox/program;jupyter notebook'


# sever alias

# du & df setting
alias du='du -h'
alias dh='dh -h'


# enable pm-suspend without sudo
alias sus='sudo pm-suspend'

alias mysql='mysql -u root -p'

# if [ -e /var/www/html/ ]; then
#    alias ch='cd /var/www/html/'	
# fi


# # build engine
# alias ncmake='rm -f CMakeCache.txt && cmake -G Ninja -DBUILD_AUTO_GENERATED_FILES=ON -DCMAKE_EXPORT_COMPILE_COMMANDS=1 ../engine/'

# android studio PATH
export PATH="${HOME}/android-studio/bin:${PATH}"
