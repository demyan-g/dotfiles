#!/bin/bash

# - Installing Homebrew
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# - Installing basics
brew bundle

chmod go-w '/usr/local/share'

# -- Installing JDKs and related tools

# - Installing vim-plug
curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
	https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
pip3 install neovim

# - Setting ZSH as Default Shell
chsh -s /usr/local/bin/zsh

# - Cleaning up existing configs
rm -rf ~/.vim ~/.vimrc ~/.zshrc ~/.config/nvim 2> /dev/null

# - Creating dirs
mkdir -p ~/.config ~/.config/nvim

# - Lining configs
ln -sf ~/dotfiles/zshrc ~/.zshrc
ln -sf ~/dotfiles/vimrc ~/.config/nvim/init.vim
ln -sf ~/dotfiles/init.el ~/.emacs.d/init.el
