#!/bin/bash

# - Installing Homebrew
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"

# - Installing basics
brew bundle

chmod go-w '/usr/local/share'

# -- Installing Emacs native-comp
brew install gcc libgccjit
brew tap d12frosted/emacs-plus
brew install emacs-plus@30 \
     --with-mailutils  --with-xwidgets --with-imagemagick --with-elrumo2-icon

# -- Installing JDKs and related tools

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
