## Environmental Variables Setting ##

# use emacs as default editor
export EDITOR="emacs"

# anaconda setting
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
export PATH="$PYENV_ROOT/versions/anaconda3-4.1.0/bin/:$PATH"

# setting for installing package in local environment
# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    export PATH="$HOME/bin:$PATH"
fi
if [ -d "$HOME/local" ] ; then
    export PATH="$HOME/local:$PATH"
fi
