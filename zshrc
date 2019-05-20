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

export PATH=/usr/local/bin:$PATH
export DYLD_LIBRARY_PATH=/usr/local/cuda/lib:$DYLD_LIBRARY_PATHexport PATH="/usr/local/sbin:$PATH"
