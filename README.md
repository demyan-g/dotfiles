# dotfiles

Personal development environment configuration for macOS, centered around **Emacs 30 + Ghostty + Zellij + Starship** with trilingual support (English/Japanese/Korean).

## Overview

| Component | Tool | Config file |
|-----------|------|-------------|
| **Editor** | Emacs 30 (emacs-plus, native-comp) | `emacs-d/` |
| **Terminal** | Ghostty | `ghostty/config` |
| **Multiplexer** | Zellij | `zellij/` |
| **Prompt** | Starship | `starship.toml` |
| **Shell** | Zsh + Antidote | `zshrc`, `zsh_plugins.txt` |
| **Package Manager** | Homebrew | `Brewfile` |
| **Editor (secondary)** | Neovim / Vim | `vimrc`, `ginit.vim` |

## Quick Start

```bash
# 1. Clone
git clone https://github.com/demyan-g/dotfiles.git ~/dotfiles
cd ~/dotfiles

# 2. Bootstrap macOS (Homebrew + core packages)
bash init_macOS.sh

# 3. Install Emacs ecosystem dependencies
bash install-dependencies.sh

# 4. Symlink configs
ln -sf ~/dotfiles/zshrc ~/.zshrc
ln -sf ~/dotfiles/starship.toml ~/.config/starship.toml
ln -sf ~/dotfiles/ghostty ~/.config/ghostty
ln -sf ~/dotfiles/zellij ~/.config/zellij
ln -sf ~/dotfiles/emacs-d ~/.emacs.d
ln -sf ~/dotfiles/vimrc ~/.config/nvim/init.vim

# 5. Set up API keys for AI features
# Create ~/.authinfo.gpg with:
#   machine api.anthropic.com login apikey password <YOUR_KEY>
#   machine api.openai.com login apikey password <YOUR_KEY>
#   machine generativelanguage.googleapis.com login apikey password <YOUR_KEY>
#   machine api.github.com login <USERNAME> password <TOKEN>
```

## Emacs Configuration

The Emacs configuration is modular, using **Elpaca** for async package management and organized into focused config files under `emacs-d/config/`:

### Architecture

```
emacs-d/
├── early-init.el          # Pre-GUI: native-comp, GC, UI stripping
├── init.el                # Bootstrap Elpaca, load modules in order
└── config/
    ├── core.el            # Encoding, backups, history, editing defaults
    ├── fonts.el           # Trilingual font setup (Sarasa Gothic)
    ├── appearance.el      # ef-themes, doom-modeline, nerd-icons
    ├── completion.el      # Vertico + Orderless + Consult + Embark + Corfu + Cape
    ├── tools.el           # Magit, Projectile, vterm, Docker, file formats
    ├── musica.el          # Apple Music control via pytunes
    ├── development.el     # Flycheck, Smartparens, YASnippet, Tree-sitter, Apheleia
    ├── lsp-config.el      # lsp-mode, lsp-ui, DAP mode
    ├── intelligence.el    # gptel (Claude, Gemini) with auth-source keys
    ├── lang-python.el     # basedpyright + ruff + uv + envrc
    ├── lang-rust.el       # rustic + rust-analyzer + codelldb
    ├── lang-java.el       # lsp-java (Eclipse JDT) + Lombok
    ├── lang-scala.el      # Metals + sbt-mode
    ├── lang-c.el          # clangd + clang-format + CMake/Meson
    ├── lang-web.el        # typescript-language-server + prettier + pnpm
    ├── org-config.el      # org-modern, olivetti, mixed-pitch
    ├── org-roam-config.el # Zettelkasten with org-roam-ui + citar
    ├── org-babel-config.el# Multi-language literate programming
    ├── org-export-config.el# PDF (XeLaTeX/CJK), HTML, GFM, Reveal.js
    ├── org-gtd.el         # GTD workflow with org-super-agenda + org-ql
    ├── input-method.el    # CJK input method keybinding fixes
    └── startup.el         # Enlight dashboard
```

### Completion Stack

**Vertico** (vertical minibuffer UI) → **Orderless** (flexible matching) → **Marginalia** (annotations) → **Consult** (enhanced commands) → **Embark** (context actions) → **Corfu** (in-buffer completion) → **Cape** (completion backends)

### Language Support

| Language | LSP Server | Formatter | Linter | Build/Package |
|----------|-----------|-----------|--------|---------------|
| Python | basedpyright | ruff | ruff | uv |
| Rust | rust-analyzer | rustfmt | clippy | cargo |
| Java | Eclipse JDT | google-java-format | JDT | Gradle/Maven |
| Scala | Metals | scalafmt | Metals | sbt |
| C/C++ | clangd | clang-format | clang-tidy | CMake/Meson |
| TypeScript/JS | ts-ls | prettier | eslint_d | pnpm |

### AI Integration

**gptel** connects to Claude (default), OpenAI, and Gemini via API keys stored in `~/.authinfo.gpg`. Keybindings under `C-c a`:

- `C-c a c` — Open chat buffer
- `C-c a s` — Send region/buffer
- `C-c a r` — Rewrite region
- `C-c a e` — Explain code at point
- `C-c a i` — Suggest improvements

### Trilingual Fonts

Uses **Sarasa Gothic** for consistent 1:2 width ratio (ASCII:CJK) across English, Japanese, and Korean. Nerd Font symbols via **Symbols Nerd Font Mono**. Apple Color Emoji as fallback.

### Org-Mode Ecosystem

- **org-modern** for clean visual styling
- **org-roam** for Zettelkasten knowledge management (with graph UI)
- **org-babel** for literate programming in 12+ languages
- **org-export** with CJK-aware XeLaTeX PDF pipeline
- **GTD workflow** via org-super-agenda + org-ql + org-edna

## Shell (Zsh)

The `zshrc` is optimized for fast startup with:

- **Antidote** plugin manager with static loading (regenerated on change)
- **Lazy loading** for `mise` and `jenv` (saves ~150ms each)
- **Homebrew GCC** auto-detection (version-agnostic aliasing)
- **Ghostty** shell integration
- **Emacs** daemon management (`sec`/`ec` functions)
- **vterm** directory tracking for Emacs integration

### Plugins (`zsh_plugins.txt`)

- `zsh-users/zsh-completions` — Additional completions
- `zsh-users/zsh-autosuggestions` — Fish-style suggestions
- `zsh-users/zsh-history-substring-search` — History search
- `agkozak/zsh-z` — Directory jumping
- `zdharma-continuum/fast-syntax-highlighting` — Syntax highlighting
- `Aloxaf/fzf-tab` — Fuzzy completion

## Terminal Stack

### Ghostty

Configured with Kanagawa Wave theme, background blur/transparency, MesloLGS Nerd Font, and Quake-style dropdown terminal (`Ctrl+`` globally). Left Option key mapped as Alt for Zellij compatibility.

### Zellij

tmux-compatible keybindings (`Ctrl-b` prefix) with Catppuccin Mocha theme. Includes a `dev` layout with editor/terminal/git split. Session serialization enabled for persistence.

## Starship Prompt

Minimal prompt showing directory, git status/metrics, language versions (only in relevant directories), and command duration. Time displayed in right prompt.

## OS Support

| Platform | Bootstrap | Status |
|----------|-----------|--------|
| macOS (Apple Silicon) | `init_macOS.sh` | Primary |
| macOS (Intel) | `init_macOS.sh` | Supported |
| Debian/Ubuntu | `install-dependencies.sh` | Emacs deps only |
| Windows | `init_Windows.bat` | Placeholder |

## Prerequisites

- macOS 13+ or Linux
- Git
- For Emacs: GCC + libgccjit (native-comp), Xcode CLI tools
- For fonts: [Sarasa Gothic](https://github.com/be5invis/Sarasa-Gothic/releases), [Symbols Nerd Font Mono](https://www.nerdfonts.com/)

## Post-Install

After first Emacs launch:
```
M-x nerd-icons-install-fonts
M-x treesit-install-language-grammar  (for each language)
```

## License

Personal configuration — use at your own discretion.
