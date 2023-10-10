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
