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

## auto-complete

autoload -Uz compinit
compinit -u

if [ -e /usr/local/share/zsh-completions ]; then
  fpath=(/usr/local/share/zsh-completions $fpath)
fi

zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
setopt list_packed
zstyle ':completion:*' list-colors ''

source $(brew --prefix)/share/zsh-autosuggestions/zsh-autosuggestions.zsh
source $(brew --prefix)/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

setopt correct

# Pyemv's Settings

export PYENV_ROOT=${HOME}/.pyenv
if [ -d "${PYENV_ROOT}" ]; then
    export PATH=${PYENV_ROOT}/bin:$PATH
    eval "$(pyenv init -)"
    eval "$(pyenv virtualenv-init -)"
fi

# Original Aliases

alias ls='ls -aG'
alias ll='ls -alG'
alias emacs='/usr/local/Cellar/emacs/26.3/bin/emacs'
alias rm='rmtrash'

# Original PATH

## homebrew

export PATH="/usr/local/sbin:$PATH"
