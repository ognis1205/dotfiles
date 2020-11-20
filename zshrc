#   --------------
#   0. SECTIONS
#   --------------
#   1. Environment Configuration
#   2. Zsh Configuration
#   3. Make Terminal Better (remapping defaults and adding functionality)

    export ZSHRC_VERSION="1.0.0"

    function zshrc_splash_screen {
        echo;
        echo -e "   ███████╗███████╗██╗  ██╗██████╗  ██████╗"
        echo -e "   ╚══███╔╝██╔════╝██║  ██║██╔══██╗██╔════╝"
        echo -e "     ███╔╝ ███████╗███████║██████╔╝██║     "
        echo -e "    ███╔╝  ╚════██║██╔══██║██╔══██╗██║     "
        echo -e "██╗███████╗███████║██║  ██║██║  ██║╚██████╗"
        echo -e "╚═╝╚══════╝╚══════╝╚═╝  ╚═╝╚═╝  ╚═╝ ╚═════╝"
        echo -e "                                           "$ZSHRC_VERSION
        echo;
    }

    zshrc_splash_screen

#   -------------------------------
#   1. ENVIRONMENT CONFIGURATION
#   -------------------------------

#   Set Paths.
    export PATH="${PATH}:/usr/local/bin/:usr/local/sbin/"
    export PATH="~/.local/bin:${PATH}"

#   Set Prompt.
#    export PROMPT='\[\033[0;33m\]\W/ \[\033[1;30m\] $ '
    if [ -d ${HOME}/.zsh/pure ] ; then
        fpath+=${HOME}/.zsh/pure
        autoload -U promptinit; promptinit
        prompt pure
    else
        PROMPT="%{${fg[green]}%}[%n@%m]%{${reset_color}%} %~
%# "
#   Set version control system information.
        autoload -Uz vcs_info
        autoload -Uz add-zsh-hook
        zstyle ':vcs_info:*' formats '%F{green}(%s)-[%b]%f'
        zstyle ':vcs_info:*' actionformats '%F{red}(%s)-[%b|%a]%f'
        function _update_vcs_info_msg() {
            LANG=en_US.UTF-8 vcs_info
            RPROMPT="${vcs_info_msg_0_}"
        }
        add-zsh-hook precmd _update_vcs_info_msg
    fi

#   Set Default Editor (change 'Nano' to the editor of your choice).
    export EDITOR=/usr/bin/nano

#   Set default blocksize for ls, df, du.
#   from this: http://hints.macworld.com/comment.php?mode=view&cid=24491
    export BLOCKSIZE=1k

#   Set locale.
    export LC_ALL=en_US.UTF-8
    export LANG=en_US.UTF-8

#   Set anyenv.
    if [ -d ${HOME}/.anyenv ] ; then
        export ANYENV_ROOT="${HOME}/.anyenv"
        export PATH="${ANYENV_ROOT}/bin:${PATH}"
        if command -v anyenv 1>/dev/null 2>&1 ; then
            eval "$(anyenv init -)"
        fi
    fi

#   Set spark.
    if [ -d /usr/local/Cellar/apache-spark ] ; then
        export SPARK_VERSION=2.2.1
        export SPARK_HOME="/usr/local/Cellar/apache-spark/${SPARK_VERSION}/libexec"
        export PYTHONPATH=/"usr/local/Cellar/apache-spark/${SPARK_VERSION}/libexec/python/:$PYTHONPATH"
    fi

#   Add color to terminal.
#   from http://osxdaily.com/2012/02/21/add-color-to-the-terminal-in-mac-os-x/
    if [ "$(uname 2> /dev/null)" != "Darwin" ]; then
        export CLICOLOR=1
        export LSCOLORS=ExFxBxDxCxegedabagacad
    fi


#   -----------------------
#   2. ZSH CONFIGURATION
#   -----------------------

#   Enable coloring.
    autoload -Uz colors
    colors

#   Set default history size.
    HISTFILE=~/.zsh_history
    HISTSIZE=10000
    SAVEHIST=10000

#   Set word delimiters.
    autoload -Uz select-word-style
    select-word-style default
    zstyle ':zle:*' word-chars " /=;@:{},|"
    zstyle ':zle:*' word-style unspecifiedx

#   Set auro complitions.
    autoload -Uz compinit
    compinit
    zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
    zstyle ':completion:*' ignore-parents parent pwd ..
    zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin /usr/sbin /usr/bin /sbin /bin /usr/X11R6/bin
    zstyle ':completion:*:processes' command 'ps x -o pid,s,args'

#   Zsh options.
    setopt print_eight_bit
    setopt no_beep
    setopt no_flow_control
    setopt ignore_eof
    setopt interactive_comments
    setopt auto_cd
    setopt auto_pushd
    setopt pushd_ignore_dups
    setopt share_history
    setopt hist_ignore_all_dups
    setopt hist_ignore_space
    setopt hist_reduce_blanks
    setopt extended_glob

#   Set keybinds.
    bindkey -e
    bindkey '^R' history-incremental-pattern-search-backward


#   --------------------------
#   3. MAKE TERMINAL BETTER
#   --------------------------

#   Custom commands.
    alias emacs='/usr/local/bin/emacs -nw'
    alias cp='cp -iv'                           # Preferred 'cp' implementation
    alias mv='mv -iv'                           # Preferred 'mv' implementation
    alias mkdir='mkdir -pv'                     # Preferred 'mkdir' implementation
    alias la='ls -FGlAhp'                       # Preferred 'ls' implementation
    alias ll='ls -FGlAhp'                       # Preferred 'ls' implementation
    alias less='less -FSRXc'                    # Preferred 'less' implementation
    cd() { builtin cd "$@"; ll; }               # Always list directory contents upon 'cd'
    alias cd..='cd ../'                         # Go back 1 directory level (for fast typers)
    alias ..='cd ../'                           # Go back 1 directory level
    alias ...='cd ../../'                       # Go back 2 directory levels
    alias .3='cd ../../../'                     # Go back 3 directory levels
    alias .4='cd ../../../../'                  # Go back 4 directory levels
    alias .5='cd ../../../../../'               # Go back 5 directory levels
    alias .6='cd ../../../../../../'            # Go back 6 directory levels
    alias edit='subl'                           # edit:         Opens any file in sublime editor
    alias f='open -a Finder ./'                 # f:            Opens current directory in MacOS Finder
    alias ~="cd ~"                              # ~:            Go Home
    alias c='clear'                             # c:            Clear terminal display
    alias which='type -af'                      # which:        Find executables
    alias path='echo -e ${PATH//:/\\n}'         # path:         Echo all executable Paths
    alias show_options='shopt'                  # Show_options: display bash options settings
    alias fix_stty='stty sane'                  # fix_stty:     Restore terminal settings when screwed up
    alias cic='set completion-ignore-case On'   # cic:          Make tab-completion case-insensitive
    mcd () { mkdir -p "$1" && cd "$1"; }        # mcd:          Makes new Dir and jumps inside
    trash () { command mv "$@" ~/.Trash ; }     # trash:        Moves a file to the MacOS trash
    ql () { qlmanage -p "$*" >& /dev/null; }    # ql:           Opens any file in MacOS Quicklook Preview
    alias DT='tee ~/Desktop/terminalOut.txt'    # DT:           Pipe content to file on MacOS Desktop

    case "$(uname 2> /dev/null)" in
        Darwin*)
            alias ls='ls -G -F'
            ;;
        Linux*)
            alias ls='ls -F --color=auto'
            ;;
    esac

#   Copy stdout to clipboard.
#   mollifier delta blog : http://mollifier.hatenablog.com/entry/20100317/p1
    if [ which pbcopy >/dev/null 2>&1 ] ; then
        alias -g C='| pbcopy'
    elif [ which xsel >/dev/null 2>&1 ] ; then
        alias -g C='| xsel --input --clipboard'
    elif [ which putclip >/dev/null 2>&1 ] ; then
        alias -g C='| putclip'
    fi
