
# zsh Settings

## language

export LANG=ja_JP.UTF-8

## history

setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_FIND_NO_DUPS
setopt HIST_REDUCE_BLANKS
setopt HIST_NO_STORE

if [ -e /usr/local/share/zsh-completions ]; then
  fpath=(/usr/local/share/zsh-completions $fpath)
fi

zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
setopt list_packed
zstyle ':completion:*' list-colors ''

source $(brew --prefix)/share/zsh-autosuggestions/zsh-autosuggestions.zsh
source $(brew --prefix)/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

setopt correct

## Prompt Setting
#
# These settings are written in .zshrc .
#

# Pyenv's Settings

export PYENV_ROOT=${HOME}/.pyenv
if [ -d "${PYENV_ROOT}" ]; then
    export PATH=${PYENV_ROOT}/bin:$PATH
    eval "$(pyenv init --path)"
    eval "$(pyenv virtualenv-init -)"
fi

# Anyenv Settings
eval "$(anyenv init -)"

# Original Aliases

alias ls='ls -aG'
alias ll='ls -alG'
alias emacs='/usr/local/bin/emacs'
alias rm='rmtrash'
alias svn='/usr/local/bin/svn'

# Original PATH

## homebrew

export PATH="/usr/local/sbin:$PATH"
export PATH="/usr/local/opt/openjdk/bin:$PATH"
export CPPFLAGS="-I/usr/local/opt/openjdk/include"

### If you need to have openssl@1.1 first in your PATH run:
export PATH="/usr/local/opt/openssl@1.1/bin:$PATH"

### For compilers to find openssl@1.1 you may need to set:
export LDFLAGS="-L/usr/local/opt/openssl@1.1/lib"
export CPPFLAGS="-I/usr/local/opt/openssl@1.1/include $CPPFLAGS"

### For pkg-config to find openssl@1.1 you may need to set:
export PKG_CONFIG_PATH="/usr/local/opt/openssl@1.1/lib/pkgconfig"

### For Node.js
export NODE_PATH=$(npm root -g) node

### For tmux
export PATH=$HOME/.tmux/bin:$PATH
