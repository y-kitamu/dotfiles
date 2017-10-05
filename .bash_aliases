# ~/.bash_aliases: list of environment variables & aliases. read in ~/.bashrc.


### environment variables ###

# use emacs as default editor
export EDITOR="emacs"

# anaconda setting
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
export PATH="$PYENV_ROOT/versions/anaconda3-4.1.0/bin/:$PATH"

# setting for installing package in local environment
# set PATH so it includes user's private directory if it exists
if [ -d "$HOME/local" ] ; then
    export PATH="$HOME/local:$PATH"
    if [ -d "$HOME/local/bin" ] ; then
       export PATH="$HOME/bin:$PATH"
    fi
    if [ -d "$HOME/local/include" ] ; then
       export CPATH="$HOME/local/include"
    fi  
fi


### aliases ###

# python & anaconda aliases
alias activate='source /home/kitamura/.pyenv/versions/anaconda3-4.1.0/bin/activate'
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'
alias jupyter='cd ~/Dropbox/jupyter;jupyter notebook'

# sever alias
alias desktop='ssh -2X kitamura@192.168.11.2'
alias step0ku='ssh -2X kitamura@130.54.59.210'
alias step1ku='ssh -2X kitamura@130.54.59.211'
alias step5ku="ssh -2X kitamura@step5ku"
alias amazon="ssh -i ~/.ssh/id_rsa ec2-user@54.199.246.210"
alias aws="ssh -i ~/.ssh/FirstKey.pem ec2-user@aws"

alias rsdata='rsync -av kitamura@step0ku:/home/kitamura/data/ /home/kitamura/data/'
alias rstep0='rsync -av kitamura@step0ku:/home/kitamura/plot/ /home/kitamura/plot/'
alias rstep5='rsync -arvz kitamura@step5ku:/home/kitamura/kmz/ /home/kitamura/kmz'
alias rspro='rsync -arvz kitamura@step0ku:/home/kitamura/VISI/ /home/kitamura/Dropbox/program/'
alias sendpro='rsync -arvz /home/kitamura/Dropbox/program/ kitamura@step0ku:/home/kitamura/VISI/'
alias portfwd="ssh a0122868@forward.kuins.kyoto-u.ac.jp -L 8080:proxy.kuins.net:8080"
w
#alias step0ku="xhost +; ssh -2X kitamura@step0ku"
#alias step1ku="xhost +; ssh -2X kitamura@step1ku"
#alias rs="rsync -arvz kitamura@step0ku:/home/kitamura/plot/ /home/kitamura/plot/"
# copy files from ssh
#rsync -aruv kitamura@step0ku:/home/kitamura/kmz /home/kitamura/

# du & df setting
alias du='du -h'
alias dh='dh -h'

# enable pm-suspend without sudo
alias sus='sudo pm-suspend'
