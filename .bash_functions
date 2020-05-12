# .bash_functions : list of custom functions. this file is read in ~/.bashrc


# cmake compile commands
function ncmake() {
    if [ $# -gt 1 ]; then
        echo "Usage : ncmake [option : <path> (default = ../)]"
    fi
    rm -rf CMakeCache.txt
    if [ $# -eq 0 ]; then
        cmake -G Ninja -DCMAKE_BUILD_TYPE=Release -DCMAKE_EXPORT_COMPILE_COMMANDS=1 ../
    else
        cmake -G Ninja -DCMAKE_BUILD_TYPE=Release -DCMAKE_EXPORT_COMPILE_COMMANDS=1 $1
    fi
}

function ncmake-debug() {
    if [ $# -gt 1 ]; then
        echo "Usage : ncmake-debug [option : <path> (default = ../)]"
    fi
    rm -rf CMakeCache.txt
    if [ $# -eq 0 ]; then
        cmake -G Ninja -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=1 ../
    else
        cmake -G Ninja -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=1 $1
    fi
}

# python virtualenv activate function
function activate() {
    subpath_to_activate_bin="./venv/bin/activate"
    if [ "${1:0:1}" = '/' ]; then
        directory=$1
    else
        directory=$(readlink -f $PWD/$1)
    fi
    while :
    do
        if [ "$directory" = "$HOME" ] || [ "$directory" = "/" ]; then
            echo "venv is not found"
            return 1
        fi
        if [ -e ${directory}/${subpath_to_activate_bin} ]; then
            echo "project root directory = ${directory}"
            source ${directory}/${subpath_to_activate_bin}
            return 0
        fi
        directory=$(readlink -f ${directory}/"../")
        if [ -z $directory ]; then
            echo "directory is empty"
            return 1
        fi
    done
}


function mkvirtualenv() {
    if [ $# -eq 0 ]; then
        activate
        if [ $? -eq 0 ]; then
            return 0
        else
            echo "You should set the project_root path"
            return 1
        fi
    elif [ $# -eq 1 ]; then
        path=$(readlink -f $1)
        venv_name=$(basename ${path})/venv
        venv_path=${path}/venv
        existing_venv=$(find ${work_dir} -name venv -type d | grep ${venv_name})
        if [ "${existing_venv}" ]; then
            echo "venv of name ${venv_name} already exists."
            venv_path=$existing_venv
        else
            python3 -m venv ${path}/venv
        fi
        activate ${venv_path}
        if [ -e ~/dotfiles/requirements.txt ]; then
            pip install -r  ~/dotfiles/requirements.txt
        fi
        return 0
    else
        "Usage : mkvirtualenv [option : <path>/<to>/<project_root>]"
    fi
}


function workon() {
    work_dir=$HOME/work
    if [ $# -eq 1 ]; then
        venv=$(find ${work_dir} -name venv -type d | grep ${1}/venv)
        venv=$(dirname ${venv})
        activate ${venv}
    else
        echo "Usage : workon <venv name>"
    fi
}


function _workon() {
    local cur prev cword
    _get_comp_words_by_ref -n : cur prev cword

    work_dir=$HOME/work
    venv_list=$(find ${work_dir} -name venv -type d | xargs -I{} bash -c 'basename ${0%/*}' {})

    COMPREPLY=($(compgen -W "${venv_list}" -- "${cur}"));
}

complete -F _workon workon

# make python project
function mkproject() {
    if [ $# -ne 1 ]; then
        echo "Usage : mkproject <project name>"
        return 1
    fi

    python3 $HOME/dotfiles/setup_project.py $1
    if [ $? -eq 0 ]; then
        mkvirtualenv $1
        cd $1
        pip install -e .
    elif [ $? -eq 1 ]; then
        rm -rf $1
    fi
}
