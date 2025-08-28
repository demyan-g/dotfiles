# PATH related
export LOCAL=$HOME/local
export GO_TOOLS=$HOME/go/bin
export PATH=/usr/local/bin:/usr/local/sbin:$GO_TOOLS:$HOMEBREW_PREFIX/opt/gnu-getopt/bin:$PATH
export PATH=$HOMEBREW_PREFIX/opt/texinfo/bin:$PATH
export PATH=$HOMEBREW_PREFIX/opt/coreutils/libexec/gnubin:$PATH
export PATH=$HOMEBREW_PREFIX/bin:$PATH
export PATH=$HOME/.jenv/bin:$PATH
export PATH=$HOMEBREW_PREFIX/opt/gnu-getopt/bin:$PATH

export DYLD_LIBRARY_PATH=/usr/local/cuda/lib:$DYLD_LIBRARY_PATH

export LDFLAGS="-L$HOMEBREW_PREFIX/opt/libffi/lib"
export CPPFLAGS="-I$HOMEBREW_PREFIX/opt/libffi/include"
export XML_CATALOG_FILES="$HOMEBREW_PREFIX/etc/xml/catalog"

export LIBRARY_PATH=$HOMEBREW_PREFIX/opt/libgccjit/lib/gcc/current${LIBRARY_PATH+:}${LIBRARY_PATH}

export OLLAMA_MODELS=/Volumes/MureHouse/__ollama-models

# export GST_PLUGIN_PATH=$HOME/.local/lib/gstreamer-1.0

# - Python related
# -- mise related
eval "$(/opt/homebrew/bin/mise activate zsh)"

# -- uv related
# eval "$(uv generate-shell-completion zsh)"
# eval "$(uvx --generate-shell-completion zsh)"

# Java related
eval "$(jenv init -)"
jdk() {
    version=$1
    export JAVA_HOME=$(/usr/libexec/java_home -v"$version");
    java -version
}
# Set Default Java in v11
jdk 11

# Enabling color
autoload colors zsh/terminfo
colors

# Prompt
precmd() { print "" }
PS1=">"
RPS1="%{$fg[magenta]%}%20<...<%~%<<%{$reset_color%}"

# Autostart Tmux
if [ "$TMUX" = "" ]; then tmux; fi

# For emacs-vterm
vterm_printf() {
    if [ -n "$TMUX" ] \
        && { [ "${TERM%%-*}" = "tmux" ] \
            || [ "${TERM%%-*}" = "screen" ]; }; then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

# Auto CD
setopt auto_cd

# Spellcheck / Typo Correction
setopt correctall
alias git status='nocorrect git status'

# zsh-git-prompt
source "$HOMEBREW_PREFIX/opt/zsh-git-prompt/zshrc.sh"

# zsh-history-substring-search
source $HOMEBREW_PREFIX/share/zsh-history-substring-search/zsh-history-substring-search.zsh

# zsh-syntax-highlighting
source $HOMEBREW_PREFIX/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
export ZSH_HIGHLIGHT_HIGHLIGHTERS_DIR=$HOMEBREW_PREFIX/share/zsh-syntax-highlighting/highlighters

# zsh-completions
if type brew &>/dev/null; then
  FPATH=$(brew --prefix)/share/zsh-completions:$FPATH
  source $(brew --prefix)/share/zsh-autosuggestions/zsh-autosuggestions.zsh

  autoload -Uz compinit && compinit
fi

rm -f ~/.zcompdump; compinit

# Package Manager - antigen
if [[ ! -f ~/dotfiles/antigen.zsh ]]; then
    curl -L git.io/antigen > ~/dotfiles/antigen.zsh
fi
source ~/dotfiles/antigen.zsh

# antigen - Syntax Highlighting
antigen bundle zsh-users/zsh-syntax-highlighting

# antigen - Autocomplete
antigen bundle zsh-users/zsh-autosuggestions

# antigen - Git Shorthand
antigen bundle git

# antigen - apply
antigen apply

# Alias
alias c='clear'
alias du='du -h'
alias df='df -h'
alias ls='ls --color=auto'
alias la='ls -lah --color=auto'
alias ll='ls -lh --color=auto'

# docker-machine env evaluate
DOCKER_MACHINE="default"
# if [[ 0 -eq `docker-machine status $DOCKER_MACHINE | grep "Running" | wc -l` ]]
# then
#     docker-machine start $DOCKER_MACHINE && eval "$(docker-machine env $DOCKER_MACHINE)"
# else
#     eval "$(docker-machine env $DOCKER_MACHINE)"
# fi

# Emacs related
sec () {
    emacs --eval "(setq server-name \"$1\")" --daemon 2>/dev/null &
}

# if  [[ 0 -eq `lsof -c emacs | grep unix | grep server | wc -l` ]]
# then
#     sec server01
# fi

ec () {
    socket_file=$(lsof -c emacs | grep unix | grep server | tr -s " " | cut -d' ' -f8)

    if [[ $socket_file == "" ]]; then        
        # Just run Emacs (with any arguments passed to the script)
        # It would be a good idea to parse the arguments and clean/remove
        # anything emacsclient specific. 
        # (ie. -e should be --eval for emacs)
        # note that emacsclient doesn't fix these args for you either
        # when using -a / --alternate-editor

        sec server01 && emacsclient $@ -nc -s server01

    else

        emacsclient $@ -nc -s $socket_file

    fi
}
