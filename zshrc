export ZSH_DISABLE_COMPFIX=true

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
export JAVA_08_HOME=$(/usr/libexec/java_home -v1.8)
export JAVA_11_HOME=$(/usr/libexec/java_home -v11)

alias java8='export JAVA_HOME=$JAVA_08_HOME'
alias java11='export JAVA_HOME=$JAVA_11_HOME'

# - default to v11
java11

# Enabling color
autoload colors zsh/terminfo
colors

# Prompt
precmd() { print "" }
PS1=">"
RPS1="%{$fg[magenta]%}%20<...<%~%<<%{$reset_color%}"

# Autostart Tmux
if [ "$TMUX" = "" ]; then tmux; fi

# jEnv initiate
if which jenv > /dev/null; then eval "$(jenv init -)"; fi

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

# Emacs related
emacs --daemon 2>/dev/null &
alias ec="emacsclient -nc"
