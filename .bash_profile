#Bash Settings
export PS1='[\t]\u@\h:\W $'
export HISTCONTROL=ignoredups

#Pyemv's Settings
export PYENV_ROOT=${HOME}/.pyenv
if [ -d "${PYENV_ROOT}" ]; then
    export PATH=${PYENV_ROOT}/bin:$PATH
    eval "$(pyenv init -)"
    eval "$(pyenv virtualenv-init -)"
fi

#Original Aliases
alias ls='ls -aG'
alias ll='ls -alG'
alias emacs='/usr/local/Cellar/emacs/26.1_1/bin/emacs'
alias rm='rmtrash'
