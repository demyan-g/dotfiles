#!/bin/bash

# - Installing Homebrew
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"

# - Installing basics
brew install \
     zsh tmux neovim python3 ag reattach-to-user-namespace \
     autoconf awscli brew-cask-completion coreutils \
     docker docker-machine ffmpeg flac ghostscript \
     git gnu-sed gnutls gzip htop imagemagick jpeg \
     jq mu msmtp neofetch parallel \
     snappy texinfo tree unzip youtube-dl zip zsh-autosuggestions \
     zsh-completions zsh-git-prompt zsh-history-substring-search \
     zsh-syntax-highlighting zstd
brew install --cask \
     cyberduck discord google-chrome google-japanese-ime \
     iterm2 microsoft-office microsoft-teams plex-media-server \
     slack spotify spotify-now-playing tunnelblick virtualbox \
     visual-studio-code vlc zoomus

chmod go-w '/usr/local/share'

# -- Installing JDKs and related tools
brew install --cask adoptopenjdk8 adoptopenjdk11 adoptopenjdk
brew install gradle && brew uninstall --ignore-dependencies openjdk

# - Installing vim-plug
curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
	https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
pip3 install neovim

# - Installing Fonts
brew install --cask font-bitstream-vera

# - Setting ZSH as Default Shell
chsh -s /usr/local/bin/zsh

# - Cleaning up existing configs
rm -rf ~/.vim ~/.vimrc ~/.zshrc ~/.tmux ~/.tmux.conf ~/.config/nvim 2> /dev/null

# - Creating dirs
mkdir -p ~/.config ~/.config/nvim

# - Lining configs
ln -s ~/dotfiles/zshrc ~/.zshrc
ln -s ~/dotfiles/tmux.conf ~/.tmux.conf
ln -s ~/dotfiles/vimrc ~/.config/nvim/init.vim
ln -s ~/dotfiles/init.el ~/.emacs.d/init.el
