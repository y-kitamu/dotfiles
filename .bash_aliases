# ~/.bash_aliases: list of custome aliases. this file is read in ~/.bashrc.

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


### aliases ###
# python & anaconda aliases
# alias activate='source /home/kitamura/.pyenv/versions/anaconda3-4.1.0/bin/activate'
# alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'
#alias jupyter='cd ~/Dropbox/program;jupyter notebook'


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

alias nbstrip_jq="jq --indent 1 \
    '(.cells[] | select(has(\"outputs\")) | .outputs) = []  \
    | (.cells[] | select(has(\"execution_count\")) | .execution_count) = null  \
    | .metadata = {\"language_info\": {\"name\": \"python\", \"pygments_lexer\": \"ipython3\"}} \
    | .cells[].metadata = {} \
    '"

# grsync -auv <sourcedir> <google drive dir> でgoogledriveへrsync
alias grsync="rsync --exclude=venv --exclude=.git --exclude=weights --exclude=runs --exclude=__pycache__ --exclude=.ipynb_checkpoints"

# updatedb excluding google drive
alias updb="sudo updatedb --prunepaths='/tmp /usr/tmp /var/tmp /afs /home/kitamura/GoogleDrive'"
