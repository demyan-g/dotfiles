#!/bin/bash
# install-dependencies.sh - Install external dependencies for Emacs configuration
# Run this script before starting Emacs with the new configuration

set -e

echo "=============================================="
echo "  Emacs Configuration Dependencies Installer"
echo "=============================================="
echo ""

# Detect OS
if [[ "$OSTYPE" == "darwin"* ]]; then
    OS="macos"
    echo "Detected: macOS"
elif [[ -f /etc/debian_version ]]; then
    OS="debian"
    echo "Detected: Debian/Ubuntu"
elif [[ -f /etc/redhat-release ]]; then
    OS="redhat"
    echo "Detected: RedHat/Fedora"
else
    OS="unknown"
    echo "Detected: Unknown OS - some commands may need adjustment"
fi

echo ""

# Function to check if command exists
command_exists() {
    command -v "$1" &> /dev/null
}

# ===========================================
# FONTS
# ===========================================
echo "=== Fonts ==="

echo "Sarasa Gothic fonts must be installed manually:"
echo "  - Download from: https://github.com/be5invis/Sarasa-Gothic/releases"
echo "  - Install Sarasa Mono J, Sarasa Mono K, Sarasa Gothic J"
echo ""

echo "Installing Nerd Fonts (Symbols only)..."
if [[ "$OS" == "macos" ]]; then
    brew install --cask font-symbols-only-nerd-font
    brew install --cask font-sarasa-gothic
else
    echo "  Download from: https://github.com/ryanoasis/nerd-fonts/releases"
    echo "  Install 'Symbols Nerd Font Mono'"
fi
echo ""

# ===========================================
# PACKAGE MANAGERS
# ===========================================
echo "=== Package Managers ==="

# pnpm
if command_exists pnpm; then
    echo "✓ pnpm already installed"
else
    echo "Installing pnpm..."
    npm install -g pnpm || corepack enable
fi

# uv (Python)
if command_exists uv; then
    echo "✓ uv already installed"
else
    echo "Installing uv..."
    curl -LsSf https://astral.sh/uv/install.sh | sh
fi

echo ""

# ===========================================
# PYTHON TOOLS
# ===========================================
echo "=== Python Tools ==="

echo "Installing basedpyright..."
uv tool install basedpyright || pip install basedpyright

echo "Installing ruff..."
uv tool install ruff || pip install ruff

echo "Installing debugpy..."
uv tool install --refresh-package debugpy debugpy 2>/dev/null || pip install debugpy

echo ""

# ===========================================
# RUST TOOLCHAIN
# ===========================================
echo "=== Rust Toolchain ==="

if command_exists rustup; then
    echo "✓ Rust already installed"
else
    echo "Installing Rust..."
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
    source "$HOME/.cargo/env"
fi

echo "Installing Rust components..."
rustup component add rust-analyzer
rustup component add clippy
rustup component add rustfmt

echo "Installing cargo-edit, cargo-expand, cargo-audit..."
cargo install cargo-edit cargo-expand cargo-audit cargo-outdated 2>/dev/null || true

echo ""

# ===========================================
# C/C++ TOOLS
# ===========================================
echo "=== C/C++ Tools ==="

if [[ "$OS" == "macos" ]]; then
    echo "Installing clangd, cmake, bear..."
    brew install llvm cmake bear
    echo "Note: Add to PATH: export PATH=\"/opt/homebrew/opt/llvm/bin:\$PATH\""
elif [[ "$OS" == "debian" ]]; then
    echo "Installing clangd, cmake, bear..."
    sudo apt-get update
    sudo apt-get install -y clangd clang-format cmake bear
fi

echo ""

# ===========================================
# TYPESCRIPT/JAVASCRIPT TOOLS
# ===========================================
echo "=== TypeScript/JavaScript Tools ==="

echo "PNPM setup"
pnpm setup
 
echo "Installing TypeScript language server..."
pnpm add -g typescript typescript-language-server

echo "Installing prettier and eslint_d..."
pnpm add -g prettier eslint_d

echo ""

# ===========================================
# JAVA
# ===========================================
echo "=== Java ==="

if [[ "$OS" == "macos" ]]; then
    echo "Installing OpenJDK..."
    brew install openjdk@17 openjdk@21
    echo "Note: Eclipse JDT will be auto-installed by lsp-java in Emacs"
elif [[ "$OS" == "debian" ]]; then
    sudo apt-get install -y openjdk-17-jdk openjdk-21-jdk
fi

echo ""

# ===========================================
# SCALA
# ===========================================
echo "=== Scala ==="

if command_exists coursier; then
    echo "✓ Coursier already installed"
else
    echo "Installing Coursier..."
    if [[ "$OS" == "macos" ]]; then
        brew install coursier/formulas/coursier
    else
        curl -fL https://github.com/coursier/launchers/raw/master/cs-x86_64-pc-linux.gz | gzip -d > cs
        chmod +x cs
        ./cs setup -y
        rm cs
    fi
fi

echo "Installing Metals..."
coursier install metals || true

echo ""

# ===========================================
# LATEX (for org-mode PDF export)
# ===========================================
echo "=== LaTeX ==="

if [[ "$OS" == "macos" ]]; then
    if command_exists xelatex; then
        echo "✓ LaTeX already installed"
    else
        echo "Installing MacTeX (this may take a while)..."
        brew install --cask mactex-no-gui
    fi
elif [[ "$OS" == "debian" ]]; then
    echo "Installing texlive..."
    sudo apt-get install -y texlive-xetex texlive-latex-extra texlive-fonts-recommended
    sudo apt-get install -y texlive-lang-cjk fonts-noto-cjk
fi

echo ""

# ===========================================
# OTHER TOOLS
# ===========================================
echo "=== Other Tools ==="

# ripgrep
if command_exists rg; then
    echo "✓ ripgrep already installed"
else
    echo "Installing ripgrep..."
    if [[ "$OS" == "macos" ]]; then
        brew install ripgrep
    elif [[ "$OS" == "debian" ]]; then
        sudo apt-get install -y ripgrep
    fi
fi

# fd
if command_exists fd; then
    echo "✓ fd already installed"
else
    echo "Installing fd..."
    if [[ "$OS" == "macos" ]]; then
        brew install fd
    elif [[ "$OS" == "debian" ]]; then
        sudo apt-get install -y fd-find
    fi
fi

# delta (git diff viewer)
if command_exists delta; then
    echo "✓ delta already installed"
else
    echo "Installing delta..."
    if [[ "$OS" == "macos" ]]; then
        brew install git-delta
    elif [[ "$OS" == "debian" ]]; then
        cargo install git-delta || true
    fi
fi

# macism (for sis on macOS)
if [[ "$OS" == "macos" ]]; then
    if command_exists macism; then
        echo "✓ macism already installed"
    else
        echo "Installing macism (for CJK input method switching)..."
        brew tap laishulu/homebrew
        brew install macism
    fi
fi

echo ""

# ===========================================
# DEBUG ADAPTERS
# ===========================================
echo "=== Debug Adapters ==="

ADAPTERS_DIR="$HOME/.emacs.d/debug-adapters"
mkdir -p "$ADAPTERS_DIR"

# codelldb for Rust/C/C++
if [[ ! -d "$ADAPTERS_DIR/codelldb" ]]; then
    echo "Installing codelldb..."
    if [[ "$OS" == "macos" ]]; then
        ARCH=$(uname -m)
        if [[ "$ARCH" == "arm64" ]]; then
            CODELLDB_URL="https://github.com/vadimcn/codelldb/releases/latest/download/codelldb-aarch64-darwin.vsix"
        else
            CODELLDB_URL="https://github.com/vadimcn/codelldb/releases/latest/download/codelldb-x86_64-darwin.vsix"
        fi
    else
        CODELLDB_URL="https://github.com/vadimcn/codelldb/releases/latest/download/codelldb-x86_64-linux.vsix"
    fi
    
    curl -L "$CODELLDB_URL" -o /tmp/codelldb.vsix
    unzip -o /tmp/codelldb.vsix -d "$ADAPTERS_DIR/codelldb"
    rm /tmp/codelldb.vsix
    echo "✓ codelldb installed"
else
    echo "✓ codelldb already installed"
fi

echo ""

# ===========================================
# EMACS LIBRARIES
# ===========================================
echo "=== Emacs Libraries ==="

LIB_DIR="$HOME/.emacs.d/lib"
mkdir -p "$LIB_DIR"

# PlantUML
if [[ ! -f "$LIB_DIR/plantuml.jar" ]]; then
    echo "Downloading PlantUML..."
    curl -L "https://github.com/plantuml/plantuml/releases/latest/download/plantuml.jar" -o "$LIB_DIR/plantuml.jar"
else
    echo "✓ PlantUML already downloaded"
fi

# Lombok
if [[ ! -f "$LIB_DIR/lombok.jar" ]]; then
    echo "Downloading Lombok..."
    curl -L "https://projectlombok.org/downloads/lombok.jar" -o "$LIB_DIR/lombok.jar"
else
    echo "✓ Lombok already downloaded"
fi

echo ""

# ===========================================
# DIRECTORIES
# ===========================================
echo "=== Creating Directories ==="

mkdir -p "$HOME/.emacs.d/backups"
mkdir -p "$HOME/.emacs.d/auto-saves/sessions"
mkdir -p "$HOME/.emacs.d/snippets"
mkdir -p "$HOME/.emacs.d/themes"
mkdir -p "$HOME/Documents/_org"
mkdir -p "$HOME/Documents/_org-roam/daily"
mkdir -p "$HOME/Documents/_org-roam/main"
mkdir -p "$HOME/Documents/_org-roam/reference"
mkdir -p "$HOME/Documents/_org-roam/projects"

echo "✓ Directories created"
echo ""

# ===========================================
# AUTHINFO REMINDER
# ===========================================
echo "=============================================="
echo "  Setup Complete!"
echo "=============================================="
echo ""
echo "Don't forget to create ~/.authinfo.gpg with your API keys:"
echo ""
echo "  machine api.openai.com login apikey password YOUR_KEY"
echo "  machine api.anthropic.com login apikey password YOUR_KEY"
echo "  machine generativelanguage.googleapis.com login apikey password YOUR_KEY"
echo "  machine api.github.com login YOUR_USERNAME password YOUR_TOKEN"
echo ""
echo "After starting Emacs, run:"
echo "  M-x nerd-icons-install-fonts"
echo "  M-x treesit-install-language-grammar (for each language)"
echo ""
echo "Enjoy your new Emacs configuration! 🚀"
