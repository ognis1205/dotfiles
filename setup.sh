#!/usr/bin/env sh
#
# Perform setup.
# Copyright (c) 2020-present, Shingo OKAWA.
# All rights reserved.


# Prints out confirmation and evaluates proceeding commands according to the confirmation answer.
# Globals:
#   None
# Arguments:
#   Confirmation message
#   Command to evaluate
if_yes_then() {
    while true; do
        read -p "${1} (y/n): " yn
        case $yn in
            [Yy]*) echo Yes ; eval "${@/${1}}" ; break ;;
            [Nn]*) echo  No ; break ;;
            *)     echo "Please enter a valid parameter (y/n): " ;;
        esac
    done
}

# Installs Xcode Command Line Tools.
# Globals:
#   None
# Arguments:
#   None
install_xcode_command_line_tools() {
    if pkgutil --pkgs=com.apple.pkg.Xcode 1>/dev/null 2>&1 ; then
        echo "Xcode Command Line Tools is already installed..." ; return
    else
        echo "Xcode Command Line Tools has not been installed. Start installing it here..."
        xcode-select --install
    fi
}

# Installs Homebrew from the github repository.
# Globals:
#   None
# Arguments:
#   None
install_homebrew() {
    if command -v brew 1>/dev/null 2>&1 ; then
        echo "Homebrew is already installed..." ; return
    else
        echo "Homebrew has not been installed. Start installing it here..."
        ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    fi
}

# Installs Anyenv from the github repository.
# Globals:
#   HOME
# Arguments:
#   None
install_anyenv() {
    if command -v anyenv 1>/dev/null 2>&1 ; then
        echo "Anyenv is already installed..." ; return
    else
        echo "Anyenv has not been installed. Start installing it here..."
        git clone https://github.com/anyenv/anyenv "${HOME}/.anyenv"
    fi
    echo 'Please make sure to add ~/.anyenv/bin to your $PATH for access to the anyenv command-line utility..., e.g.,'
    cat << 'EOF'
if [ -d ${HOME}/.anyenv ] ; then
    export ANYENV_ROOT="${HOME}/.anyenv"
    export PATH="${ANYENV_ROOT}/bin:${PATH}"
    if command -v anyenv 1>/dev/null 2>&1 ; then
        eval "$(anyenv init -)"
    fi
fi
EOF
}

# Installs Tmux with Homebrew.
# Globals:
#   None
# Arguments:
#   None
install_tmux() {
    if command -v tmux 1>/dev/null 2>&1 ; then
        echo "Tmux is already installed..." ; return
    else
        echo "Tmux has not been installed. Start installing it here..."
        brew install tmux
    fi
}

# Deploys dotfiles from the current directory.
# Globals:
#   HOME
# Arguments:
#   None
deploy_dotfiles() {
    paths="$(find $(pwd) -maxdepth 1 \( -iname "\.*" ! -iname ".gitignore" ! -iname ".git" \) -print)"
    for path in "${paths[@]}" ; do
        dotfile="$(basename ${path})"
        if [ -f "${HOME}/${dotfile}" ] || [ -d "${HOME}/${dotfile}" ] ; then
            backup="${HOME}/${dotfile}.bk.$(date +%F_%R)"
            echo "Renaming existing ${HOME}/${dotfile} to ${backup}..."
            mv "${HOME}/${dotfile}" "${backup}"
        fi
        echo "Creating symbolic link to ${path} under home directory..."
        ln -s "${path}" "${HOME}/${dotfile}"
    done
}


if_yes_then "Do you want to install Xcode Command Line Tools?" install_xcode_command_line_tools
if_yes_then "Do you want to install Homebrew?" install_homebrew
if_yes_then "Do you want to install Anyenv?" install_anyenv
if_yes_then "Do you want to install Tmux?" install_tmux
if_yes_then "Do you want to deploy dotfiles?" deploy_dotfiles
