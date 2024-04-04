#
# Executes commands at the start of an interactive session.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# Customize to your needs...

## theme

autoload -Uz promptinit
promptinit
prompt agnoster theme

## language

export LANG=ja_JP.UTF-8

## Original PATH

### homebrew

export PATH="/usr/local/sbin:$PATH"
export PATH="/usr/local/opt/openjdk/bin:$PATH"
export PATH="/opt/homebrew/bin:$PATH"
export CPPFLAGS="-I/usr/local/opt/openjdk/include"

#### If you need to have openssl@1.1 first in your PATH run:
export PATH="/usr/local/opt/openssl@1.1/bin:$PATH"

#### For compilers to find openssl@1.1 you may need to set:
export LDFLAGS="-L/usr/local/opt/openssl@1.1/lib"
export CPPFLAGS="-I/usr/local/opt/openssl@1.1/include $CPPFLAGS"

#### For pkg-config to find openssl@1.1 you may need to set:
export PKG_CONFIG_PATH="/usr/local/opt/openssl@1.1/lib/pkgconfig"

#### For tmux
export PATH=$HOME/.tmux/bin:$PATH

### history

setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_FIND_NO_DUPS
setopt HIST_REDUCE_BLANKS
setopt HIST_NO_STORE

### aliases

alias ls='ls -aG'
alias ll='ls -alG'
alias emacs='/opt/homebrew/bin/emacs'
alias rm='trash'
alias svn='/opt/homebrew/bin/svn'

### mise

eval "$(/opt/homebrew/bin/mise activate zsh)"
