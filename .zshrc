# -------------------------------
# 0. SECTIONS
# -------------------------------
# 1. Environment Configuration
# 2. Zsh Configuration
# 3. Functions and Aliases
# 4. Development Environment

export ZSHRC_VERSION=1.0.0
printf "\e[31m\n\e[0m"
printf "\e[31m   ███████╗███████╗██╗  ██╗██████╗  ██████╗\n\e[0m"
printf "\e[31m   ╚══███╔╝██╔════╝██║  ██║██╔══██╗██╔════╝\n\e[0m"
printf "\e[31m     ███╔╝ ███████╗███████║██████╔╝██║     \n\e[0m"
printf "\e[31m    ███╔╝  ╚════██║██╔══██║██╔══██╗██║     \n\e[0m"
printf "\e[31m██╗███████╗███████║██║  ██║██║  ██║╚██████╗\n\e[0m"
printf "\e[31m╚═╝╚══════╝╚══════╝╚═╝  ╚═╝╚═╝  ╚═╝ ╚═════╝\n\e[0m"
printf "\e[31m                                           ${ZSHRC_VERSION}\n\e[0m"
printf "\e[31m\n\e[0m"


# -------------------------------
# 1. ENVIRONMENT CONFIGURATION
# -------------------------------

source ~/.zsh.d/env.conf

# -------------------------------
# 2. ZSH CONFIGURATION
# -------------------------------

source ~/.zsh.d/zsh.conf

# -------------------------------
# 3. FUNCTIONS AND ALIASES
# -------------------------------

source ~/.zsh.d/functions.sh

source ~/.zsh.d/aliases.conf


# -------------------------------
# 4. DEVELOPMENT ENVIRONMENT
# -------------------------------

source ~/.zsh.d/dev.conf
