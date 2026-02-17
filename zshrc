#!/usr/bin/env zsh
# ~/.zshrc - Modernized configuration
# Migrated from oh-my-zsh/antigen to Antidote
# Optimized for Ghostty + Zellij + Starship

# ═══════════════════════════════════════════════════════════
# HOMEBREW - Cache prefix to avoid slow subshell on every start
# ═══════════════════════════════════════════════════════════

# Hardcode for Apple Silicon (avoids 40ms `brew --prefix` call)
if [[ -d /opt/homebrew ]]; then
    export HOMEBREW_PREFIX="/opt/homebrew"
elif [[ -d /usr/local/Homebrew ]]; then
    export HOMEBREW_PREFIX="/usr/local"
fi

# ═══════════════════════════════════════════════════════════
# HOMEBREW GCC - Version-agnostic configuration
# ═══════════════════════════════════════════════════════════

# Dynamically find the latest gcc version installed via Homebrew
# Works with gcc-14, gcc-15, gcc-16, etc.
# NOTE: Does NOT override $CC/$CXX — keeps clang as default compiler
#       But `gcc` and `g++` commands will use Homebrew's gcc
_setup_homebrew_gcc() {
    local gcc_opt_dir="$HOMEBREW_PREFIX/opt/gcc"
    
    # Check if Homebrew gcc is installed
    [[ -d "$gcc_opt_dir" ]] || return 0
    
    # Find the highest version of gcc available
    local latest_gcc
    latest_gcc=$(ls "$HOMEBREW_PREFIX/bin"/gcc-* 2>/dev/null | grep -E 'gcc-[0-9]+$' | sort -t- -k2 -n | tail -1)
    
    if [[ -n "$latest_gcc" ]]; then
        local gcc_version="${latest_gcc##*-}"  # Extract version number (e.g., "15")
        
        # Export version for other tools to use
        export HOMEBREW_GCC_VERSION="$gcc_version"
        
        # Export GCC/GXX for explicit use in scripts/builds
        export GCC="$HOMEBREW_PREFIX/bin/gcc-$gcc_version"
        export GXX="$HOMEBREW_PREFIX/bin/g++-$gcc_version"
        
        # Add gcc bin directory to PATH
        path=("$gcc_opt_dir/bin" $path)
        
        # Make `gcc` and `g++` commands use Homebrew's gcc (version-agnostic)
        # NOTE: $CC/$CXX remain unset — clang is still the default compiler
        alias gcc="$GCC"
        alias g++="$GXX"
    fi
}

# Run gcc setup
_setup_homebrew_gcc

# ═══════════════════════════════════════════════════════════
# PATH CONFIGURATION
# ═══════════════════════════════════════════════════════════

# Use typeset -U to deduplicate PATH entries
typeset -U path

path=(
    $HOME/.local/bin
    $HOMEBREW_PREFIX/bin
    $HOMEBREW_PREFIX/sbin
    $HOMEBREW_PREFIX/opt/coreutils/libexec/gnubin
    $HOMEBREW_PREFIX/opt/gnu-getopt/bin
    $HOMEBREW_PREFIX/opt/llvm/bin
    $HOMEBREW_PREFIX/opt/texinfo/bin
    $HOME/go/bin
    $HOME/.jenv/bin
    /usr/local/bin
    /usr/local/sbin
    $path
)
export PATH

# ═══════════════════════════════════════════════════════════
# ENVIRONMENT VARIABLES
# ═══════════════════════════════════════════════════════════

export EDITOR="emacs"
export VISUAL="emacs"
export PAGER="less"
export LANG="en_US.UTF-8"

# XDG Base Directories
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"

# Build flags
export LDFLAGS="-L$HOMEBREW_PREFIX/opt/libffi/lib"
export CPPFLAGS="-I$HOMEBREW_PREFIX/opt/libffi/include"
export XML_CATALOG_FILES="$HOMEBREW_PREFIX/etc/xml/catalog"

# Emacs native-comp support (uses "current" symlink, version-agnostic)
export LIBRARY_PATH="$HOMEBREW_PREFIX/opt/libgccjit/lib/gcc/current${LIBRARY_PATH:+:$LIBRARY_PATH}"

# Ensure libgccjit.so symlink exists for Emacs native-comp
if [[ -f "$HOMEBREW_PREFIX/opt/libgccjit/lib/gcc/current/libgccjit.dylib" ]] && \
   [[ ! -e "$HOMEBREW_PREFIX/opt/libgccjit/lib/gcc/current/libgccjit.so" ]]; then
    ln -s libgccjit.dylib "$HOMEBREW_PREFIX/opt/libgccjit/lib/gcc/current/libgccjit.so" 2>/dev/null
fi

# Ollama models location
export OLLAMA_MODELS="/Volumes/MureHouse/__ollama-models"

# pnpm
export PNPM_HOME="$HOME/Library/pnpm"
[[ ":$PATH:" != *":$PNPM_HOME:"* ]] && path=($PNPM_HOME $path)

# ═══════════════════════════════════════════════════════════
# HISTORY
# ═══════════════════════════════════════════════════════════

HISTFILE="${XDG_DATA_HOME:-$HOME/.local/share}/zsh/history"
HISTSIZE=50000
SAVEHIST=50000

# Create history directory if needed
[[ -d "${HISTFILE:h}" ]] || mkdir -p "${HISTFILE:h}"

setopt EXTENDED_HISTORY          # Write timestamp to history
setopt HIST_EXPIRE_DUPS_FIRST    # Expire duplicates first
setopt HIST_IGNORE_DUPS          # Don't record duplicates
setopt HIST_IGNORE_ALL_DUPS      # Remove old duplicates
setopt HIST_IGNORE_SPACE         # Don't record commands starting with space
setopt HIST_FIND_NO_DUPS         # Don't show duplicates in search
setopt HIST_REDUCE_BLANKS        # Remove superfluous blanks
setopt HIST_VERIFY               # Show command before executing from history
setopt SHARE_HISTORY             # Share history between sessions
setopt INC_APPEND_HISTORY        # Add commands immediately

# ═══════════════════════════════════════════════════════════
# ZSH OPTIONS
# ═══════════════════════════════════════════════════════════

setopt AUTO_CD                   # cd by typing directory name
setopt AUTO_PUSHD                # Push directory to stack
setopt PUSHD_IGNORE_DUPS         # Don't push duplicates
setopt PUSHD_SILENT              # Don't print directory stack
setopt CORRECT                   # Command correction (not correctall - too aggressive)
setopt INTERACTIVE_COMMENTS      # Allow comments in interactive shell
setopt NO_BEEP                   # Don't beep on errors
setopt EXTENDED_GLOB             # Extended globbing
setopt GLOB_DOTS                 # Include dotfiles in globbing

# ═══════════════════════════════════════════════════════════
# KEY BINDINGS
# ═══════════════════════════════════════════════════════════

bindkey -e                       # Emacs keybindings (matches your editor preference)

# History search with arrow keys
bindkey '^[[A' history-search-backward
bindkey '^[[B' history-search-forward
bindkey '^P' history-search-backward
bindkey '^N' history-search-forward

# Word navigation
bindkey '^[[1;5C' forward-word   # Ctrl+Right
bindkey '^[[1;5D' backward-word  # Ctrl+Left

# Home/End
bindkey '^[[H' beginning-of-line
bindkey '^[[F' end-of-line
bindkey '^A' beginning-of-line
bindkey '^E' end-of-line

# Delete
bindkey '^[[3~' delete-char

# ═══════════════════════════════════════════════════════════
# ANTIDOTE PLUGIN MANAGER
# ═══════════════════════════════════════════════════════════

# Install Antidote if not present
ANTIDOTE_HOME="${XDG_DATA_HOME:-$HOME/.local/share}/antidote"
if [[ ! -d "$ANTIDOTE_HOME" ]]; then
    print -P "%F{33}▓▒░ Installing Antidote…%f"
    git clone --depth=1 https://github.com/mattmc3/antidote.git "$ANTIDOTE_HOME"
fi

# Source antidote
source "$ANTIDOTE_HOME/antidote.zsh"

# Define plugins in a heredoc (or use ~/.zsh_plugins.txt file)
# Using static loading for speed - run `antidote bundle < ~/.zsh_plugins.txt > ~/.zsh_plugins.zsh` after changes
zsh_plugins="${ZDOTDIR:-$HOME}/.zsh_plugins"
if [[ ! -f "${zsh_plugins}.zsh" ]] || [[ "${zsh_plugins}.txt" -nt "${zsh_plugins}.zsh" ]]; then
    # Generate static file if missing or outdated
    [[ -f "${zsh_plugins}.txt" ]] || cat > "${zsh_plugins}.txt" << 'EOF'
# Core plugins
zsh-users/zsh-completions kind:fpath
zsh-users/zsh-autosuggestions
zsh-users/zsh-history-substring-search

# Syntax highlighting (must be last)
zdharma-continuum/fast-syntax-highlighting
EOF
    antidote bundle < "${zsh_plugins}.txt" > "${zsh_plugins}.zsh"
fi
source "${zsh_plugins}.zsh"

# ═══════════════════════════════════════════════════════════
# COMPLETIONS (single compinit call, properly secured)
# ═══════════════════════════════════════════════════════════

# Add Homebrew completions to fpath BEFORE compinit
fpath=($HOMEBREW_PREFIX/share/zsh/site-functions $HOMEBREW_PREFIX/share/zsh-completions $fpath)

# Initialize completion system (ONCE only)
autoload -Uz compinit

# Only regenerate dump once per day for speed
if [[ -n ${ZDOTDIR:-$HOME}/.zcompdump(#qN.mh+24) ]]; then
    compinit
else
    compinit -C  # Use cached dump
fi

# Completion styling
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'  # Case-insensitive
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*' menu select
zstyle ':completion:*' special-dirs true
zstyle ':completion:*' squeeze-slashes true

# Caching
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path "${XDG_CACHE_HOME:-$HOME/.cache}/zsh/zcompcache"
[[ -d "${XDG_CACHE_HOME:-$HOME/.cache}/zsh" ]] || mkdir -p "${XDG_CACHE_HOME:-$HOME/.cache}/zsh"

# ═══════════════════════════════════════════════════════════
# AUTOSUGGESTIONS CONFIG
# ═══════════════════════════════════════════════════════════

ZSH_AUTOSUGGEST_STRATEGY=(history completion)
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
ZSH_AUTOSUGGEST_USE_ASYNC=1
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#666666"

# Accept suggestion with Ctrl+Space
bindkey '^ ' autosuggest-accept

# History substring search bindings
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down

# ═══════════════════════════════════════════════════════════
# LAZY LOADING - DEVELOPMENT TOOLS
# ═══════════════════════════════════════════════════════════

# mise (lazy) - replaces synchronous eval
if command -v mise &>/dev/null; then
    mise() {
        unset -f mise
        eval "$(/opt/homebrew/bin/mise activate zsh)"
        mise "$@"
    }
    # Or if you need it active immediately, use:
    # eval "$(/opt/homebrew/bin/mise activate zsh)"
fi

# jenv (lazy) - saves ~100ms on startup
if [[ -d "$HOME/.jenv" ]]; then
    jenv() {
        unset -f jenv
        eval "$(command jenv init -)"
        jenv "$@"
    }
    # Java version helper
    jdk() {
        # Ensure jenv is loaded
        if type jenv | grep -q 'function'; then
            eval "$(command jenv init -)"
        fi
        local version=${1:-11}
        export JAVA_HOME=$(/usr/libexec/java_home -v"$version" 2>/dev/null)
        [[ -n "$JAVA_HOME" ]] && java -version
    }
fi

# ═══════════════════════════════════════════════════════════
# ALIASES
# ═══════════════════════════════════════════════════════════

# Core
alias c='clear'
alias du='du -h'
alias df='df -h'
alias ls='ls --color=auto'
alias la='ls -lah --color=auto'
alias ll='ls -lh --color=auto'
alias grep='grep --color=auto'
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'

# Git (basic - Antidote's git plugin adds more)
alias gs='git status'
alias ga='git add'
alias gc='git commit'
alias gp='git push'
alias gl='git log --oneline -20'
alias gd='git diff'
alias gco='git checkout'
alias gb='git branch'

# Disable correction for specific commands
alias git='nocorrect git'
alias cp='nocorrect cp'
alias mv='nocorrect mv'
alias mkdir='nocorrect mkdir'

# Zellij shortcuts
alias zj='zellij'
alias zja='zellij attach'
alias zjl='zellij list-sessions'

# ═══════════════════════════════════════════════════════════
# EMACS INTEGRATION
# ═══════════════════════════════════════════════════════════

# Start Emacs daemon with custom server name
sec() {
    local name="${1:-server}"
    emacs --eval "(setq server-name \"$name\")" --daemon 2>/dev/null &
}

# Smart emacsclient - starts server if needed
ec() {
    local socket_file
    socket_file=$(lsof -c emacs 2>/dev/null | grep unix | grep server | tr -s " " | cut -d' ' -f8 | head -1)

    if [[ -z "$socket_file" ]]; then
        sec server01 && sleep 0.5 && emacsclient "$@" -nc -s server01
    else
        emacsclient "$@" -nc -s "$socket_file"
    fi
}

# For emacs-vterm (Zellij-compatible version, tmux code removed)
vterm_printf() {
    printf "\e]%s\e\\" "$1"
}

# vterm directory tracking
if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    vterm_prompt_end() {
        vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
    }
    PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'
fi

# ═══════════════════════════════════════════════════════════
# GHOSTTY SHELL INTEGRATION
# ═══════════════════════════════════════════════════════════

if [[ -n "$GHOSTTY_RESOURCES_DIR" ]]; then
    source "$GHOSTTY_RESOURCES_DIR/shell-integration/zsh/ghostty-integration"
fi

# ═══════════════════════════════════════════════════════════
# ZELLIJ AUTO-ATTACH (optional - uncomment to enable)
# ═══════════════════════════════════════════════════════════

# Auto-attach to Zellij session when opening Ghostty
# if [[ -n "$GHOSTTY_RESOURCES_DIR" && "$TERM" = "xterm-ghostty" && -z "$ZELLIJ" ]]; then
#     zellij attach -c main
# fi

# ═══════════════════════════════════════════════════════════
# STARSHIP PROMPT (must be last)
# ═══════════════════════════════════════════════════════════

# Using Starship instead of zsh-git-prompt and custom PS1/RPS1
if command -v starship &>/dev/null; then
    eval "$(starship init zsh)"
else
    # Fallback prompt if Starship not installed
    precmd() { print "" }
    PS1=">"
    RPS1="%{$fg[magenta]%}%20<...<%~%<<%{$reset_color%}"
    autoload colors zsh/terminfo && colors
fi
