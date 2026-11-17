# Executes commands at the start of an interactive session.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

## shell basics

### history

HISTFILE="${ZDOTDIR:-$HOME}/.zsh_history"
HISTSIZE=100000
SAVEHIST=100000
setopt EXTENDED_HISTORY
setopt SHARE_HISTORY
setopt INC_APPEND_HISTORY

### directory

setopt AUTO_CD
setopt AUTO_PUSHD
setopt PUSHD_IGNORE_DUPS
setopt PUSHD_SILENT

### misc

setopt INTERACTIVE_COMMENTS
setopt NO_BEEP
# Ctrl-S による端末のフリーズを防ぐ
setopt NO_FLOW_CONTROL

bindkey -e
autoload -Uz select-word-style && select-word-style bash

## plugins

# compinit より前に fpath を揃える必要があるため、補完の初期化はプラグイン読み込み後に行う
fpath=(
  $HOME/.docker/completions(N)
  /opt/homebrew/share/zsh/site-functions(N)
  $fpath
)

if (( $+commands[sheldon] )); then
  eval "$(sheldon source)"
fi

### completion

autoload -Uz compinit
# 補完定義のキャッシュは 1 日 1 回だけ作り直し、起動を速く保つ
if [[ -n ${ZDOTDIR:-$HOME}/.zcompdump(N.mh+24) ]]; then
  compinit
else
  compinit -C
fi

zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}' 'r:|[._-]=* r:|=*'
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path "${XDG_CACHE_HOME:-$HOME/.cache}/zsh/zcompcache"

### history-substring-search

if (( $+functions[history-substring-search-up] )); then
  bindkey '^[[A' history-substring-search-up
  bindkey '^[[B' history-substring-search-down
  bindkey '^P' history-substring-search-up
  bindkey '^N' history-substring-search-down
fi

### prompt

if (( $+commands[starship] )); then
  eval "$(starship init zsh)"
fi

## language

export LANG=ja_JP.UTF-8

## PATH

# /opt/homebrew/{bin,sbin} と /usr/local/{bin,sbin} は .zprofile で設定済み
# (N) は存在しないディレクトリを取り除く
typeset -U path
path=(
  $HOME/.antigravity/antigravity/bin(N)
  /opt/homebrew/opt/libpq/bin(N)
  $HOME/.tmux/bin(N)
  $path
)

## openssl@1.1

# openssl@1.1 は 2023-09-11 にサポート終了。通常は openssl@3 が使われる。
# 1.1 に依存するビルドが必要になったときだけ以下を有効にする。
# Cellar の実体パスはバージョン更新で壊れるため opt のリンクを指すこと。
# export LDFLAGS="-L/opt/homebrew/opt/openssl@1.1/lib ${LDFLAGS}"
# export CPPFLAGS="-I/opt/homebrew/opt/openssl@1.1/include ${CPPFLAGS}"
# export PKG_CONFIG_PATH="/opt/homebrew/opt/openssl@1.1/lib/pkgconfig:${PKG_CONFIG_PATH}"
# path=(/opt/homebrew/opt/openssl@1.1/bin $path)

# openjdk は未導入のため環境変数の設定は不要（必要になれば brew install openjdk）

### history

setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_FIND_NO_DUPS
setopt HIST_REDUCE_BLANKS
setopt HIST_NO_STORE

### aliases

alias ls='ls -aG'
alias ll='command ls -alG'
alias rm='trash'
alias -s md='leaf'

### mise

eval "$(/opt/homebrew/bin/mise activate zsh)"

### pipenv

export PIPENV_VENV_IN_PROJECT=true

### weasyprint

# weasyprint は各プロジェクトの .venv から import されるため、
# ラッパー関数では捕捉できない。グローバルに設定する必要がある。
export DYLD_FALLBACK_LIBRARY_PATH=/opt/homebrew/lib:$DYLD_FALLBACK_LIBRARY_PATH

## local settings

# 会社の設定など公開できないものは ~/.zshrc.local に置き、リポジトリでは管理しない
[[ -r ~/.zshrc.local ]] && source ~/.zshrc.local

export CURL_CA_BUNDLE="/opt/homebrew/etc/ca-certificates/cert.pem"
[[ -r ~/.tmux/plugins/tmux-claude-continuity/claude-continuity.zsh ]] && source ~/.tmux/plugins/tmux-claude-continuity/claude-continuity.zsh
