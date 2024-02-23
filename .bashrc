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
cd ~  # move to home directory

# Define color codes
ESC=$(printf '\033')            # escape sequence (echoコマンド内で使用)
GREEN=$(printf '\033[32m')
RED=$(printf '\033[31m')
NC=$(printf '\033[0m')  # No color

# Define functions to print messages using the color codes
function echo_done() {
    echo "${ESC}${GREEN} Done. ${ESC}[m"
}

function echo_skip() {
    echo "${ESC}${RED} Skip. ${ESC}[m"
}

function echo_failed() {
    echo "${ESC}${RED} Failed. ${ESC}[m"
}

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    echo -n "Load .bash_aliases ..."
    . ~/.bash_aliases
    echo_done
fi

# Function definitions
if [ -f ~/.bash_functions ]; then
    echo -n "Load .bash_functions ..."
    . ~/.bash_functions
    echo_done
fi


# add local bin path
echo -n "Set up local bin path ... "
if [ -e ${HOME}/.local/bin ]; then
    export PATH=${HOME}/.local/bin${PATH:+:${PATH}}
    export PYTHONPATH=${HOME}/.local/bin${PYTHONPATH:+:${PYTHONPATH}}
    echo_done
else
    echo_skip
fi


echo -n "Set up custom keybindings ..."
if [ $(loginctl show-session 2 -p Type) = "Type=x11" ] && [ -e ~/dotfiles/load_xkbmap.sh ]; then
    source ~/dotfiles/load_xkbmap.sh && echo_done || echo_failed
elif [ $(loginctl show-session 2 -p Type) = "Type=wayland" ] && [ -e ~/.local/bin/xremap ] && [ -e ~/dotfiles/xremap_config.yml ]; then
    pkill xremap
    nohup xremap ~/dotfiles/xremap_config.yml & &> /dev/null
else
    echo_skip
fi

### environment variables ###
# use emacs as default editor
export EDITOR="emacs"

# if [ -f /usr/local/bin/virtualenvwrapper.sh ]; then
#     export WORKON_HOME=$HOME/.pyenv
#     if [ -f /usr/bin/python3 ]; then
#         export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python3
#     fi
#     source /usr/local/bin/virtualenvwrapper.sh
# fi

# if [ -e ~/work/ ]; then
#     export PYTHONPATH=${HOME}/work/${PYTHONPATH:+:${PYHTHONPATH}}
# fi

# OS customize setting
if [ "$(uname)" == 'Darwin' ]; then
    # for Mac
    echo -n "Set up Darwin settings ... "
    if [ -e ~/dotfiles/.bashrc_mac ]; then
        # echo "read settings for Mac"
        . ~/dotfiles/.bashrc_mac
        echo_done
    else
        echo_skip
    fi
elif [ "$(expr substr $(uname -s) 1 5)" == 'Linux' ]; then
    # for Linux
    if [ -e ~/dotfiles/.bashrc_linux ]; then
        . ~/dotfiles/.bashrc_linux
    fi
fi

# rust
echo -n "Set up rust settings ... "
export RUSTUP_HOME=${HOME}/.rustup
export CARGO_HOME=${HOME}/.cargo
if [ -e $CARGO_HOME/env ]; then
    source "$CARGO_HOME/env"

    RUST_COMPLETION_DIR=~/.local/share/bash-completion/completions
    if [ ! -e ${RUST_COMPLETION_DIR} ]; then
        mkdir -p ${RUST_COMPLETION_DIR}
    fi
    rustup completions bash > ${RUST_COMPLETION_DIR}/rustup
    rustup completions bash cargo >${RUST_COMPLETION_DIR}/cargo
    source ${RUST_COMPLETION_DIR}/rustup
    source ${RUST_COMPLETION_DIR}/cargo

    echo_done
else
    echo_skip
fi

# cask (emacs package management tool)
echo -n "Set up emacs cask path ... "
if [ -e $HOME/.cask ]; then
    export PATH=${HOME}/.cask/bin${PATH:+:${PATH}}
    echo_done
else
    echo_skip
fi

# android studio path
if [ -e //usr/local/android-studio/bin ]; then
    export PATH=/usr/local/android-studio/bin${PATH:+:${PATH}}
fi

# for gsettings error
export GIO_EXTRA_MODULES=/usr/lib/x86_64-linux-gnu/gio/modules

echo -n "Set up google cloud SDK ... "
# The next line updates PATH for the Google Cloud SDK.
if [ -f '/home/kitamura/google-cloud-sdk/path.bash.inc' ]; then
    . '/home/kitamura/google-cloud-sdk/path.bash.inc'
    echo_done
else
    echo_skip
fi

# The next line enables shell command completion for gcloud.
if [ -f '/home/kitamura/google-cloud-sdk/completion.bash.inc' ]; then
    . '/home/kitamura/google-cloud-sdk/completion.bash.inc'
fi

# cuda
echo -n "Set up cuda path ... "
if [ -e /usr/local/cuda ]; then
    export PATH="/usr/local/cuda/bin:$PATH"
    export LD_LIBRARY_PATH="/usr/local/cuda/extras/CUPTI/lib64:/usr/local/cuda/compat/lib:/usr/local/nvidia/lib:/usr/local/nvidia/lib64:$LD_LIBRARY_PATH"
    echo_done
else
    echo_skip
fi

# add time info to history command
export HISTTIMEFORMAT='%Y-%m-%d %T%z '

# deno
echo -n "Set up deno ... "
export DENO_INSTALL="${HOME}/.deno"
if [ -e "${DENO_INSTALL}" ]; then
    export PATH="$DENO_INSTALL/bin:$PATH"
    echo_done
else
    echo_skip
fi

# poetry config
echo -n "Set up poetry ... "
if command -v poetry > /dev/null; then
    if [ ! -e "${HOME}/.config/pypoetry/config.toml" ]; then
        poetry config virtualenvs.in-project true
        echo_done
    else
        echo_skip
    fi
else
    echo_skip
fi

# export NPM_CONFIG_PREFIX="/usr/local"

# vterm config
vterm_printf(){
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

# ros setup
ROS_SETUP_SCRIPT=/opt/ros/humble/setup.bash
if [ -e ${ROS_SETUP_SCRIPT} ]; then
    source ${ROS_SETUP_SCRIPT}
fi
unset ROS_SETUP_SCRIPT
# colcon setup
COLCON_CD=/usr/share/colcon_cd/function/colcon_cd.sh
if [ -e ${COLCON_CD} ]; then
    source ${COLCON_CD}
    export _colcon_cd_root=/opt/ros/humble
fi
COLCON_ARG_COMP=/usr/share/colcon_argcomplete/hook/colcon-argcomplete.bash
if [ -e ${COLCON_ARG_COMP} ]; then
    source ${COLCON_ARG_COMP}
fi


# bash history integration
function share_history {
    history -a  # .bash_historyに前回コマンドを1行追記
    history -c  # 端末ローカルの履歴を一旦消去
    history -r  # .bash_historyから履歴を読み込み直す
}
PROMPT_COMMAND='share_history'  # 上記関数をプロンプト毎に自動実施
shopt -u histappend   # .bash_history追記モードは不要なのでOFFに
export HISTSIZE=99999  # 履歴のMAX保存数を指定

# aws setup
# AWSのAPIキーファイルが存在する場合は環境変数に設定する
echo -n "Set up aws environment variables ... "
AWS_ENV_FILE=${HOME}/.aws/credentials
if [ -e ${AWS_CREDENTIAL_FILE} ]; then
    export AWS_ACCESS_KEY_ID=`grep aws_access_key_id ${AWS_ENV_FILE} | awk '{print $3}'`
    export AWS_SECRET_ACCESS_KEY=`grep aws_secret_access_key ${AWS_ENV_FILE} | awk '{print $3}'`
    if [ $? -eq 0 ]; then
        echo_done
    else
        echo_failed
    fi
else
    echo_skip
fi

# mount google drive
alias mount_gdrive="rclone mount gdrive: ~/remote/gdrive/ &"
if [ -e ${HOME}/remote/gdrive ]; then
    echo -n "Mount google drive ... "
    if [ -z "$(ls -A ${HOME}/remote/gdrive)" ]; then
        mount_gdrive
        echo_done
    else
        echo_skip
    fi
fi

# set open-api secret to environment variable
echo -n "Set up open-api secret ... "
SCRIPT_DIR=$( cd -- "$( dirname -- $(realpath "${BASH_SOURCE[0]}" ))" &> /dev/null && pwd )
SECRET_FILE=${SCRIPT_DIR}/secrets.sh
if [ -e ${SECRET_FILE} ]; then
    source ${SECRET_FILE}
    echo_done
else
    echo_skip
fi

# For emcacs configuration
export LSP_USE_PLISTS=true
. "$HOME/.cargo/env"
