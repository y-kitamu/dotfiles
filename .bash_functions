# .bash_functions : list of custom functions. this file is read in ~/.bashrc


# cmake compile commands
function ncmake() {
    if [ $# -gt 1 ]; then
        echo "Usage : ncmake [option : <path> (default = ../)]"
    fi
    rm -rf CMakeCache.txt
    if [ $# -eq 0 ]; then
        cmake -G Ninja -DCMAKE_BUILD_TYPE=Release -DCMAKE_EXPORT_COMPILE_COMMANDS=1 ../
    fi
    cmake -G Ninja -DCMAKE_BUILD_TYPE=Release -DCMAKE_EXPORT_COMPILE_COMMANDS=1 $1
}

function ncmake-debug() {
    if [ $# -gt 1 ]; then
        echo "Usage : ncmake-debug [option : <path> (default = ../)]"
    fi
    rm -rf CMakeCache.txt
    if [ $# -eq 0 ]; then
        cmake -G Ninja -DCMAKE_BUILD_TYPE=Debug DCMAKE_EXPORT_COMPILE_COMMANDS=1 ../
        cmake -G Ninja -DCMAKE_BUILD_TYPE=Release -DCMAKE_EXPORT_COMPILE_COMMANDS=1 ../
    fi
    cmake -G Ninja -DCMAKE_BUILD_TYPE=Debug DCMAKE_EXPORT_COMPILE_COMMANDS=1 $1
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
        venv_name=$(basename $1)/venv
        venv_path=$1/venv
        existing_venv=$(find ${work_dir} -name venv -type d | grep ${venv_name})
        if [ ${existing_venv} ]; then
            echo "venv of name ${venv_name} already exists."
            venv_path=$existing_venv
        else
            python3 -m venv $1/venv
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
        echo "project root directory = ${venv}"
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
