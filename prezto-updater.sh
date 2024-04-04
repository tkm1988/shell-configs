#! /bin/zsh

source $HOME/.zshrc

cd $ZPREZTODIR
git pull
git submodule sync --recursive
git submodule update --init --recursive

exit 0
