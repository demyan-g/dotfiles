# PATH related
export LOCAL=$HOME/local
export PATH=/usr/local/sbin:$HOME/.jenv/bin:$PATH
export DYLD_LIBRARY_PATH=/usr/local/cuda/lib:$DYLD_LIBRARY_PATH

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
fpath=(/usr/local/share/zsh-completions $fpath)
#rm -f ~/.zcompdump; compinit

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
