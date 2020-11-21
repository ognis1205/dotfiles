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
            [Yy]* ) echo "Yes"; eval ${@/$1}; break;;
            [Nn]* ) echo "No"; break;;
            * ) echo "Please enter a valid parameter (y/n): ";;
        esac
    done
}

# Deploys dotfiles from the current directory.
# Globals:
#   HOME
# Arguments:
#   None
deploy_dotfiles() {
    paths=$(find $(pwd) -maxdepth 1 \( -iname "\.*" ! -iname ".gitignore" ! -iname ".git" \) -print)
    for path in ${paths[@]} ; do
        dotfile=$(basename ${path})
        if [ -f ${HOME}/${dotfile} ] || [ -d ${HOME}/${dotfile} ] ; then
            echo "Renamed existing ${HOME}/${dotfile} to ${backup}..."
            backup=${HOME}/${dotfile}.bk.$(date +%F_%R)
            mv ${HOME}/${dotfile} ${backup}
        fi
        ln -s ${path} ${HOME}/${dotfile}
        echo "Creating symbolic link to ${path} under home directory..."
    done
}

# Installs Anyenv from the github repository.
# Globals:
#   HOME
# Arguments:
#   None
install_anyenv() {
    if [ ! -d ${HOME}/.anyenv ] ; then
        echo "Anyenv hasn't been installed. Start installing it here..."
        git clone https://github.com/anyenv/anyenv ${HOME}/.anyenv
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


if_yes_then "Do you want to deploy dotfiles?" deploy_dotfiles
if_yes_then "Do you want to install anyenv?" install_anyenv
