#!/bin/bash

# - Installing basics
brew install zsh tmux neovim/neovim/neovim python3 ag reattach-to-user-namespace
brew tap caskroom/cask
brew cask install iterm2

# - Installing vim-plug
curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
	https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
pip2 install neovim
pip3 install neovim

# - Installing Fonts
brew tap caskroom/fonts
brew cask install font-bitstream-vera

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

