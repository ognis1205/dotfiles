#!/usr/bin/env sh
#
# Perform setup.
# Copyright (c) 2020-present, Shingo OKAWA.
# All rights reserved.


# Prints out information.
# Globals:
#   None
# Arguments:
#   Message
info() {
    printf "\e[32m[INFO]\e[0m ${*}"
}

# Prints out confirmation and evaluates proceeding commands according to the confirmation answer.
# Globals:
#   None
# Arguments:
#   Confirmation message
#   Command to evaluate
if_yes_then() {
    while true; do
        info "${1}" ; read -p " (y/n): " yn
        case "${yn}" in
            [Yy]*) info "Yes\n" ; eval "${@/${1}}" ; break ;;
            [Nn]*) info  "No\n" ; break ;;
            *)     info "Please enter a valid parameter (y/n).\n" ;;
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
        info "Xcode Command Line Tools is already installed...\n" ; return
    else
        info "Xcode Command Line Tools has not been installed. Start installing it here...\n"
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
        info "Homebrew is already installed...\n" ; return
    else
        info "Homebrew has not been installed. Start installing it here...\n"
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
        info "Anyenv is already installed...\n" ; return
    else
        info "Anyenv has not been installed. Start installing it here...\n"
        git clone https://github.com/anyenv/anyenv "${HOME}/.anyenv"
    fi
    info 'Please make sure to add ~/.anyenv/bin to your $PATH for access to the anyenv command-line utility..., e.g.,\n'
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
        info "Tmux is already installed...\n" ; return
    else
        info "Tmux has not been installed. Start installing it here...\n"
        brew install tmux
    fi
}

install_wget() {
    echo
}

# Deploys dotfiles from the current directory.
# Globals:
#   HOME
# Arguments:
#   None
deploy_dotfiles() {
    paths="$(find $(pwd) -maxdepth 1 \( -iname "\.*" ! -iname ".gitignore" ! -iname ".git" \) -print)"
    for path in ${paths[@]} ; do
        dotfile="$(basename ${path})"
        if [ -f "${HOME}/${dotfile}" ] || [ -d "${HOME}/${dotfile}" ] ; then
            backup="${HOME}/${dotfile}.bk.$(date +%F_%R)"
            info "Renaming existing ${HOME}/${dotfile} to ${backup}...\n"
            mv "${HOME}/${dotfile}" "${backup}"
        fi
        info "Creating symbolic link to ${path} under home directory...\n"
        ln -s "${path}" "${HOME}/${dotfile}"
    done
}


if_yes_then\
    "Do you want to install Xcode Command Line Tools?"\
    install_xcode_command_line_tools

if_yes_then\
    "Do you want to install Homebrew?"\
    install_homebrew

if_yes_then\
    "Do you want to install Anyenv?"\
    install_anyenv

if_yes_then\
    "Do you want to install Tmux?"\
    install_tmux

if_yes_then\
    "Do you want to deploy dotfiles?"\
    deploy_dotfiles

info "\xF0\x9F\x8D\xBB...Done!\n"
