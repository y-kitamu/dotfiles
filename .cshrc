### Environmental Variables ###

# Use emacs
setenv EDITOR emacs

#IDL path setting
setenv IDL_PATH /usr/local/bin/idl
setenv IDL_STARTUP /home/kitamura/.idl/init.pro

#Python environment 
setenv PATH "$PATH":/usr/local/bin
setenv PYENV_ROOT "$HOME/".pyenv
setenv PATH  "$PYENV_ROOT"/bin:"$PATH"
setenv PATH /home/step/kitamura/.pyenv/shims:"$PATH"
setenv PYENV_SHELL tcsh
# conda py2 environment
setenv PATH "$PYENV_ROOT"/versions/anaconda3-4.1.0/envs/py2/bin/:"$PATH"
# conda root environment
setenv PATH "$PYENV_ROOT"/versions/anaconda3-4.1.0/bin/:"$PATH"
setenv CONDA_ENVS_PATH "$PYENV_ROOT"/versions/anaconda3-4.1.0/envs

# use python VISI programs
setenv VISIROOT "$HOME"/program
setenv PYTHONPATH "$VISIROOT"/Empirical_Model:"$VISIROOT"/VISI_Modules:"$VISIROOT"/LT_Lat:"$VISIROOT"/Lon_Lat:"$VISIROOT"/Geophysical_Index:"$VISIROOT"/Sami2:"$VISIROOT"/cythonlib

# To use packages installed on local directory.
setenv PATH "$HOME"/local/usr/bin:"$HOME"/local/bin:"$PATH"
#setenv CPATH "$HOME"/local/usr/include:"$HOME"/local/include:"$HOME"/.pyenv/versions/anaconda3-4.1.0/include/python3.5m
#setenv LD_LIBRARY_PATH "$HOME"/local/usr/lib64:"$HOME"/local/lib:/usr/lib64


### aliases ###

# ls aliases
if ( -x /usr/bin/dircolors ) then
    alias ls 'ls --color=auto'
endif
alias ll 'ls -alF'
alias la 'ls -A'
alias l 'ls -CF'

# du & df setting
alias du 'du -h'
alias dh 'dh -h'

# GCC
alias g++ 'g++ -std=c++11'

# server alias
alias step0ku 'ssh -2X kitamura@130.54.59.210'
alias step1ku 'ssh -2X kitamura@130.54.59.211'
alias step5ku "ssh -2X kitamura@step5ku"

alias rsdata 'rsync -av kitamura@step0ku:/home/kitamura/data /home/kitamura/data/'
alias rstep0 'rsync -av kitamura@step0ku:/home/kitamura/plot/ /home/kitamura/plot/'
alias rstep5 'rsync -arvz kitamura@step5ku:/home/kitamura/kmz/ /home/kitamura/kmz'
alias rspro 'rsync -arvz kitamura@step0ku:/home/kitamura/VISI/ /home/kitamura/Dropbox/program/'
alias sendpro 'rsync -arvz /home/kitamura/Dropbox/program/ kitamura@step0ku:/home/kitamura/VISI/'

#alias step0ku="xhost +; ssh -2X kitamura@step0ku"
#alias step1ku="xhost +; ssh -2X kitamura@step1ku"
#alias rs="rsync -arvz kitamura@step0ku:/home/kitamura/plot/ /home/kitamura/plot/"
# copy files from ssh
#rsync -aruv kitamura@step0ku:/home/kitamura/kmz /home/kitamura/

#source ${HOME}/cron/git_cron.sh > /dev/null
cd 
