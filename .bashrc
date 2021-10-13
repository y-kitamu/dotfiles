# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

# for emacs multi-term to be colorful
case "$TERM" in
    eterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;31m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# sizebook local aliases
if [ -f ~/.bash_sb_aliases ]; then
    . ~/.bash_sb_aliases
fi

# Function definitions
if [ -f ~/.bash_functions ]; then
    . ~/.bash_functions
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi



############# CUSTOM SETTING ##############

if [ -e ~/dotfiles/load_xkbmap.sh ]; then
    source ~/dotfiles/load_xkbmap.sh
fi

### environment variables ###
# use emacs as default editor
export EDITOR="emacs"

if [ -f /usr/local/bin/virtualenvwrapper.sh ]; then
    export WORKON_HOME=$HOME/.pyenv
    if [ -f /usr/bin/python3 ]; then
        export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python3
    fi
    source /usr/local/bin/virtualenvwrapper.sh
fi

if [ -e ~/work/ ]; then
    export PYTHONPATH=${HOME}/work/${PYTHONPATH:+:${PYHTHONPATH}}
fi

# OS customize setting
if [ "$(uname)" == 'Darwin' ]; then
    # for Mac
    if [ -e ~/dotfiles/.bashrc_mac ]; then
        # echo "read settings for Mac"
        . ~/dotfiles/.bashrc_mac
    fi
elif [ "$(expr substr $(uname -s) 1 5)" == 'Linux' ]; then
    # for Linux
    if [ -e ~/dotfiles/.bashrc_linux ]; then
        . ~/dotfiles/.bashrc_linux
    fi
fi

# rust
if [ -e $HOME/.cargo/env ]; then
    source "$HOME/.cargo/env"
fi

# cask (emacs package management tool)
if [ -e $HOME/.cask ]; then
    export PATH=${HOME}/.cask/bin${PATH:+:${PATH}}
fi

# android studio path
if [ -e //usr/local/android-studio/bin ]; then
    export PATH=/usr/local/android-studio/bin${PATH:+:${PATH}}
fi

# to use droidcam
if [ -f /usr/lib/x86_64-linux-gnu/libv4l/v4l2convert.so ]; then
    export LD_PRELOAD=/usr/lib/x86_64-linux-gnu/libv4l/v4l2convert.so
fi

# add local bin path
if [ -e ${HOME}/.local/bin ]; then
    export PATH=${HOME}/.local/bin${PATH:+:${PATH}}
    export PYTHONPATH=${HOME}/.local/bin${PYTHONPATH:+:${PYTHONPATH}}
fi

# for gsettings error
export GIO_EXTRA_MODULES=/usr/lib/x86_64-linux-gnu/gio/modules

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/home/kitamura/google-cloud-sdk/path.bash.inc' ]; then . '/home/kitamura/google-cloud-sdk/path.bash.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/home/kitamura/google-cloud-sdk/completion.bash.inc' ]; then . '/home/kitamura/google-cloud-sdk/completion.bash.inc'; fi

# bash history integration
function share_history {
    history -a  # .bash_historyに前回コマンドを1行追記
    history -c  # 端末ローカルの履歴を一旦消去
    history -r  # .bash_historyから履歴を読み込み直す
}
PROMPT_COMMAND='share_history'  # 上記関数をプロンプト毎に自動実施
shopt -u histappend   # .bash_history追記モードは不要なのでOFFに
export HISTSIZE=99999  # 履歴のMAX保存数を指定

# cuda
if [ -e /usr/local/cuda ]; then
    export PATH="/usr/local/cuda/bin:$PATH"
    export LD_LIBRARY_PATH="/usr/local/cuda/lib64:$LD_LIBRARY_PATH"
fi

# add time info to history command
export HISTTIMEFORMAT='%Y-%m-%d %T%z '

# xonsh
function create_xonsh_env {
    if [ ! -e ${HOME}/.venv ]; then
        mkdir ${HOME}/.venv
    fi
    python3 -m venv ${HOME}/.venv/xonsh
    source ${HOME}/.venv/xonsh/bin/activate
    sudo apt install xsel
    pip install xonsh[full]
}

# if [ -e ${HOME}/.venv/xonsh ]; then
#     source ${HOME}/.venv/xonsh/bin/activate
#     xonsh
# fi

# if [ -e /opt/ros/noetic/setup.bash ]; then
#     source /opt/ros/noetic/setup.bash
# fi
# source /opt/ros/galactic/setup.bash
