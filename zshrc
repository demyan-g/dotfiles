# PATH related
export LOCAL=$HOME/local
export GO_TOOLS=$HOME/go/bin
export PATH=/usr/local/bin:/usr/local/sbin:$GO_TOOLS:/usr/local/opt/gnu-getopt/bin:$PATH
export PATH=/usr/local/opt/texinfo/bin:$PATH
export PATH=/usr/local/opt/coreutils/libexec/gnubin:$PATH
export DYLD_LIBRARY_PATH=/usr/local/cuda/lib:$DYLD_LIBRARY_PATH

export LDFLAGS="-L/usr/local/opt/libffi/lib"
export CPPFLAGS="-I/usr/local/opt/libffi/include"
export XML_CATALOG_FILES="/usr/local/etc/xml/catalog"

# Java related
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

# Auto CD
setopt auto_cd

# Spellcheck / Typo Correction
setopt correctall
alias git status='nocorrect git status'

# zsh-git-prompt
source "/usr/local/opt/zsh-git-prompt/zshrc.sh"

# zsh-history-substring-search
source /usr/local/share/zsh-history-substring-search/zsh-history-substring-search.zsh

# zsh-syntax-highlighting
source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
export ZSH_HIGHLIGHT_HIGHLIGHTERS_DIR=/usr/local/share/zsh-syntax-highlighting/highlighters

# zsh-completions
if type brew &>/dev/null; then
  FPATH=$(brew --prefix)/share/zsh-completions:$FPATH

  autoload -Uz compinit
  compinit
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

# docker-machine env evaluate
DOCKER_MACHINE="default"
if [[ 0 -eq `docker-machine status $DOCKER_MACHINE | grep "Running" | wc -l` ]]
then
    docker-machine start $DOCKER_MACHINE && eval "$(docker-machine env $DOCKER_MACHINE)"
else
    eval "$(docker-machine env $DOCKER_MACHINE)"
fi

# Emacs related
sec () {
    emacs --eval "(setq server-name \"$1\")" --daemon 2>/dev/null &
}

if  [[ 0 -eq `lsof -c emacs | grep unix | grep server | wc -l` ]]
then
    sec server01
fi

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
