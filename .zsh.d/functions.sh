# Always list directory contents upon 'cd'.
cd () {
    builtin cd "$@";
    ll;
}

# Makes new Dir and jumps inside.
mcd () {
    mkdir -p "$1" && cd "$1";
}

# Moves a file to the MacOS trash.
trash () {
    command mv "$@" ~/.Trash ;
}

# Opens any file in MacOS Quicklook Preview.
ql () {
    qlmanage -p "$*" >& /dev/null;
}

# Checks my IP.
ip () {
    curl http://checkip.amazonaws.com;
}

# Tmux helper functions.
tx () {
    session="${1}"
    if [ "$session" == "ls" ]; then
        tmux ls
        return
    fi
    if [ ! -z "${TMUX}" ]; then
        tmux switch-client -t "$session"
        return $?
    fi
    tmux ls | grep "^${session}:" -q
    if [ $? -eq 0 ]; then
        tmux attach -t "$session"
    else
        tmux new -s "$session"
    fi
}

# Terraform helper functions.
tfdestroy () {
    terraform state list | sed "s/.*/'&'/" | xargs -L 1 terraform state rm;
}

# Programming.
elisp () {
    emacs -batch -l "${1}";
}

atcc () {
    g++ -DLOCAL -std=c++11 -g "./${1}" -o "./${1%.*}" && "./${1%.*}" "./${1%.*}.in";
}

atpy () {
    python "${1}" --input "${1%.*}.in";
}

atja () {
    javac  -Xdiags:verbose -Xlint:unchecked "${1}" && java "${1%.*}" "${1%.*}.in" ;
}

# Misc.
rgb2hex(){
    printf '#'
    for var in "$@"; do
        printf '%02X' "$var"
    done
    printf '\n'
}

cached_eval() {
    cache="/tmp/${2}"
    if [[ ! -e "${cache}" ]]; then
        eval "${1}" > "${cache}"
        zcompile "${cache}"
    fi
    source "${cache}"
}
