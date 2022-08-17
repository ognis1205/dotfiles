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

# Customizing OSX configuration.
# Globals:
#   None
# Arguments:
#   None
customize_osx() {
    info "Setting menu clock...\n"
    info "See 'http://www.unicode.org/reports/tr35/tr35-31/tr35-dates.html#Date_Format_Patterns'...\n"
    defaults write com.apple.menuextra.clock "DateFormat" 'EEE MMM d  h:mm:ss a'
    killall SystemUIServer
    info "Setting the dock to hide automatically...\n"
    defaults write com.apple.dock autohide -bool true
    killall Dock
    info "Fast key repeat rate, requires reboot to take effect...\n"
    defaults write ~/Library/Preferences/.GlobalPreferences KeyRepeat -int 1
    defaults write ~/Library/Preferences/.GlobalPreferences InitialKeyRepeat -int 15
    info "Setting finder to display full path in title bar...\n"
    defaults write com.apple.finder '_FXShowPosixPathInTitle' -bool true
    info "Stop Photos from opening automatically...\n"
    defaults -currentHost write com.apple.ImageCapture disableHotPlug -bool true
    info "Modify appearance of dock: remove standard icons, add chrome and iTerm...\n"
    if [ ! command -v dockutil 1>/dev/null 2>&1 ] ; then
        info "Dockutil has not been installed. Start installing it here...\n"
        curl https://raw.githubusercontent.com/kcrawford/dockutil/master/scripts/dockutil > /usr/local/bin/dockutil
    fi
    chmod a+rx,go-w /usr/local/bin/dockutil
    #dockutil --list | awk -F\t '{print "dockutil --remove \""$1"\" --no-restart"}' | sh
    #dockutil --add /System/Applications/Launchpad.app
    #dockutil --add /Applications/Xcode.app
    #dockutil --add /System/Applications/Utilities/Terminal.app
    #dockutil --add /System/Applications/App%20Store.app
    #dockutil --add /System/Applications/System%20Preferences.app
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
    info "Ensuring you have the latest Homebrew...\n"
    brew update
    info "Ensuring your Homebrew directory is writable...\n"
    sudo chown -Rf $(whoami) $(brew --prefix)/*
    info "Installing Homebrew services...\n"
    brew tap homebrew/services
    info "Adding Pivotal tap to Homebrew...\n"
    brew tap pivotal/tap
    info "Upgrading existing brews...\n"
    brew upgrade
    info "Cleaning up your Homebrew installation...\n"
    brew cleanup
    info "Installing Homebrew cask...\n"
    brew install cask
    info 'Please make sure to add ~/.xshrc to your $PATH for access to the homebrew command-line utility..., e.g.,\n'
    cat << 'EOF'
'export PATH="/usr/local/sbin:$PATH"' >> ~/.zshrc
EOF
}

# Installs Git with Homebrew.
# Globals:
#   None
# Arguments:
#   None
install_git() {
    if command -v git 1>/dev/null 2>&1 ; then
        info "Git is already installed...\n" ; return
    else
        info "Git has not been installed. Start installing it here...\n"
        brew install git
    fi
    hooks_dir="${HOME}/Documents/github/git-hooks-core"
    if [ ! -d ${hooks_dir} ]; then
        info "Installing git hooks for cred-alert...\n"
        # for more information see https://github.com/pivotal-cf/git-hooks-core
        git clone https://github.com/pivotal-cf/git-hooks-core ${hooks_dir}
        git config --global --add core.hooksPath ${hooks_dir}
    else
        info "Updating git-hooks for cred-alert...\n"
        pushd ${hooks_dir}
        git pull -r
        popd
    fi
    if command -v cred-alert-cli 1>/dev/null 2>&1 ; then
        info "Cred Alert CLI is already installed...\n" ; return
    else
        info "Cred Alert CLI has not been installed. Start installing it here...\n"
        os=$(uname | awk '{print tolower($1)}')
        curl -o cred-alert-cli https://s3.amazonaws.com/cred-alert/cli/current-release/cred-alert-cli_${os}
        chmod 755 cred-alert-cli
        mv cred-alert-cli /usr/local/bin
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

# Installs tfenv with Homebrew.
# Globals:
#   HOME
# Arguments:
#   None
install_tfenv() {
    if command -v tfenv 1>/dev/null 2>&1 ; then
        info "tfenv is already installed...\n" ; return
    else
        info "tfenv has not been installed. Start installing it here...\n"
	brew install tfenv
    fi
    info 'Please make sure to configure git secrets, e.g.,\n'
    cat << 'EOF'
% cat ~/.gitconfig
[secrets]
    providers = git secrets --aws-provider
    patterns = (A3T[A-Z0-9]|AKIA|AGPA|AIDA|AROA|AIPA|ANPA|ANVA|ASIA)[A-Z0-9]{16}
    patterns = (\"|')?(AWS|aws|Aws)?_?(SECRET|secret|Secret)?_?(ACCESS|access|Access)?_?(KEY|key
aws_access_key_id = "AKIAIOSFODNN7EXAMPLE"
|Key)(\"|')?\\s*(:|=>|=)\\s*(\"|')?[A-Za-z0-9/\\+=]{40}(\"|')?
    patterns = (\"|')?(AWS|aws|Aws)?_?(ACCOUNT|account|Account)_?(ID|id|Id)?(\"|')?\\s*(:|=>|=)\\s*(\"|')?[0-9]{4}\\-?[0-9]{4}\\-?[0-9]{4}(\"|')?
    allowed = AKIAIOSFODNN7EXAMPLE
    allowed = wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY
EOF
}

# Installs Rust compiler from the offcial repository.
# Globals:
#   HOME
# Arguments:
#   None
install_rust() {
    if command -v rustc 1>/dev/null 2>&1 ; then
        info "Rust compiler is already installed...\n" ; return
    else
        info "Rust compiler has not been installed. Start installing it here...\n"
	curl --proto '=https' --tlsv1.2 https://sh.rustup.rs -sSf | sh
    fi
    info 'Please make sure to add ~/.cargo/bin to your $PATH for access to the cargo command-line utility..., e.g.,\n'
    cat << 'EOF'
if [ -d "${HOME}/.cargo" ] ; then
    export CARGO_ROOT="${HOME}/.cargo"
    export PATH="${CARGO_ROOT}/bin:${PATH}"
fi
EOF
}

# Installs GCC with Homebrew.
# Globals:
#   None
# Arguments:
#   None
install_gcc() {
    if ls -d /usr/local/bin/* | grep "g++-" ; then
        info "GCC is already installed...\n" ; return
    else
        info "GCC has not been installed. Start installing it here...\n"
        brew install gcc
        ln -sf $(ls -d /usr/local/bin/* |  grep '^/usr/local/bin/g++-\d+*' | sort -r | head -n1) /usr/local/bin/g++
        ln -sf $(ls -d /usr/local/bin/* |  grep '^/usr/local/bin/gcc-\d+*' | sort -r | head -n1) /usr/local/bin/gcc
    fi
    info 'Please make sure to add ~/.anyenv/bin to your $PATH for access to the anyenv command-line utility..., e.g.,\n'
    cat << 'EOF'
export CC=/usr/local/bin/gcc
export CXX=/usr/local/bin/g++
EOF
    if command -v cmake 1>/dev/null 2>&1 ; then
        info "Cmake is already installed...\n" ; return
    else
        info "Cmake has not been installed. Start installing it here...\n"
        brew install cmake
    fi
    if command -v gdb 1>/dev/null 2>&1 ; then
        info "gdb is already installed...\n" ; return
    else
        info "gdb has not been installed. Start installing it here...\n"
        brew install gdb
    fi
    if [ -e /usr/local/include/boost ] ; then
        info "Boost is already installed...\n" ; return
        version=$(echo -e '#include <boost/version.hpp>\nBOOST_VERSION' | g++ - -I/usr/local/include -E -P)
        info "Your Boost versions is ${version}...\n"
    else
        info "Boost has not been installed. Start installing it here...\n"
        brew install boost
    fi
    if [ -e /usr/local/include/gtest ] && [ -e /usr/local/include/gmock ] ; then
        info "GoogleTest is already installed...\n" ; return
    else
        info "Boost has not been installed. Start installing it here...\n"
	info "Creating temporary working directory ${work_dir}...\n"
	work_dir=$(mktemp -d)
	git clone https://github.com/google/googletest "${work_dir}"
	$(cd "${work_dir}/googletest" && mkdir build && cd build && cmake .. && make && make install)
	info "Deleting temporary working directory ${work_dir}...\n"
	rm -rf "${work_dir}"
    fi
    if command -v llvm-g++ 1>/dev/null 2>&1 ; then
        info "LLVM is already installed...\n" ; return
    else
        info "LLVM has not been installed. Start installing it here...\n"
        brew install llvm
    fi
    info 'Please make sure to add ~/.xshrc to your $PATH for access to the homebrew command-line utility..., e.g.,\n'
    cat << 'EOF'
'export PATH="/usr/local/opt/llvm/bin:$PATH"' >> ~/.zshrc
EOF
    info 'Trying to install Doxygen with support of Graphviz...\n'
    if command -v doxygen 1>/dev/null 2>&1 ; then
        info "Doxygen is already installed...\n" ; return
    else
        info "Doxygen has not been installed. Start installing it here...\n"
        brew install doxygen
    fi
    if command -v dot 1>/dev/null 2>&1 ; then
        info "Graphviz is already installed...\n" ; return
    else
        info "Graphvbiz has not been installed. Start installing it here...\n"
	brew install libtool
	brew link libtool
	brew install graphviz
	brew link --overwrite graphviz
    fi
    info 'Trying to install pkg-config...\n'
    if command -v pkg-config 1>/dev/null 2>&1 ; then
        info "pkg-config is already installed...\n" ; return
    else
        info "pkg-config has not been installed. Start installing it here...\n"
        brew install pkg-config
    fi
    info 'Trying to install flawfinder...\n'
    if command -v flawfinder 1>/dev/null 2>&1 ; then
        info "flawfinder is already installed...\n" ; return
    else
        info "flawfinder has not been installed. Start installing it here...\n"
        brew install flawfinder
    fi
}

# Installs GPerfTools.
# Globals:
#   HOME
# Arguments:
#   None
install_gperftools() {
    #brew install google-perftools graphviz ghostscript gv
    #brew tap denismm/gv
}

# Installs Javascript related IDEs.
# Globals:
#   HOME
# Arguments:
#   None
install_javascript_ide() {
}

# Installs Docker with Homebrew.
# Globals:
#   None
# Arguments:
#   None
install_docker() {
    if command -v docker 1>/dev/null 2>&1 ; then
        info "Docker is already installed...\n" ; return
    else
        info "Docker has not been installed. Start installing it here...\n"
        brew cask install docker
    fi
    info "Stop Docker gracefully...\n"
    test -z "$(docker ps -q 2>/dev/null)" && osascript -e 'quit app "Docker"'
    info "Start Docker gracefully...\n"
    open --background -a Docker
}

# Installs Language Servers.
# Globals:
#   None
# Arguments:
#   None
install_language_servers() {
    # TODO: Fix Metals' configuration to be editable.
    info "LLVM/Clangd is installed in GCC section...\n"
    info "For emacs, you can install lsp-mode that includes lsp-pyright...\n"
    if command -v metals-emacs 1>/dev/null 2>&1 ; then
        info "Metals is already installed...\n" ; return
    else
        info "Metals has not been installed. Start installing it here...\n"
	curl -L -o coursier https://git.io/coursier-cli
	chmod +x coursier
	./coursier bootstrap\
		   --java-opt -Xss4m\
		   --java-opt -Xms100m\
		   --java-opt -Dmetals.client=emacs\
		   org.scalameta:metals_2.12:0.9.7\
		   -r bintray:scalacenter/releases\
		   -r sonatype:snapshots \
		   -o /usr/local/bin/metals-emacs -f
    if command -v bloop 1>/dev/null 2>&1 ; then
        info "Bloop is already installed...\n" ; return
    else
        info "Bloop has not been installed. Start installing it here...\n"
	brew install scalacenter/bloop/bloop
    fi
    if command -v typescript-language-server 1>/dev/null 2>&1 ; then
	info "typescript-language-server is already installed...\n" ; return
    else
	if command -v npm 1>/dev/null 2>&1 ; then
	    info "typescript-language-server has not been installed. Start installing it here...\n"
	    npm install -g typescript-language-server
	    npm i -g tern
	else
	    info "Installing typescript-language-server requires npm, please check the installation of Javascript related IDEs...\n"
	fi
    fi
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

# Installs useful commands with Homebrew.
# Globals:
#   None
# Arguments:
#   None
install_commands() {
    if command -v wget 1>/dev/null 2>&1 ; then
        info "wget is already installed...\n" ; return
    else
        info "wget has not been installed. Start installing it here...\n"
        brew install wget
    fi
    if command -v jq 1>/dev/null 2>&1 ; then
        info "jq is already installed...\n" ; return
    else
        info "jq has not been installed. Start installing it here...\n"
        brew install jq
    fi
}

# Installs useful NPM packages.
# Globals:
#   None
# Arguments:
#   None
install_npm_packages() {
    if command -v typescript-language-server 1>/dev/null 2>&1 ; then
        info "typescript-language-server is already installed...\n" ; return
    else
        info "typescript-language-server has not been installed. Start installing it here...\n"
        npm install -g typescript-language-server
    fi
    if command -v prettier 1>/dev/null 2>&1 ; then
        info "prittier is already installed...\n" ; return
    else
        info "prittier has not been installed. Start installing it here...\n"
        npm install -g --save-dev --save-exact prettier
    fi
    if command -v ts-node 1>/dev/null 2>&1 ; then
        info "ts-node is already installed...\n" ; return
    else
        info "ts-node has not been installed. Start installing it here...\n"
	npm install -g ts-node
    fi
}

# Deploys dotfiles from the current directory.
# Globals:
#   HOME
# Arguments:
#   None
deploy_dotfiles() {
    paths="$(find $(pwd) -maxdepth 1 \( -iname "\.*" ! -iname ".gitignore" ! -iname ".gitmodules" ! -iname ".git" \) -print)"
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
    "Do you want to customize OSX?"\
    customize_osx

if_yes_then\
    "Do you want to install Homebrew?"\
    install_homebrew

if_yes_then\
    "Do you want to install Git?"\
    install_git

if_yes_then\
    "Do you want to install Anyenv?"\
    install_anyenv

if_yes_then\
    "Do you want to install GCC?"\
    install_gcc

if_yes_then\
    "Do you want to install Docker?"\
    install_docker

if_yes_then\
    "Do you want to install Language Servers?"\
    install_language_servers

if_yes_then\
    "Do you want to install Tmux?"\
    install_tmux

if_yes_then\
    "Do you want to install useful commands?"\
    install_commands

if_yes_then\
    "Do you want to install npm packages?"\
    install_npm_packages

if_yes_then\
    "Do you want to deploy dotfiles?"\
    deploy_dotfiles

info "\xF0\x9F\x8D\xBB...Done!\n"
