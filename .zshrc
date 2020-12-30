autoload -Uz colors && colors

PROMPT='%{${fg[green]}%}%n@%m : %c %{${reset_color}%} %{${fg[red]}%}%# %{${reset_color}%}'
RPROMPT='%{${fg[green]}%}[%D %t]%{${reset_color}%}'


### vcs Setting

autoload -Uz vcs_info
precmd () { vcs_info }
setopt prompt_subst
zstyle ':vcs_info:git:*' check-for-changes true #formats 設定項目で %c,%u が使用可
zstyle ':vcs_info:git:*' stagedstr "%F{green}!" #commit されていないファイルがある
zstyle ':vcs_info:git:*' unstagedstr "%F{magenta}+" #add されていないファイルがある
zstyle ':vcs_info:*' formats "%F{cyan}%c%u(%b)%f" #通常
zstyle ':vcs_info:*' actionformats '[%b|%a]' #rebase 途中,merge コンフリクト等 formats 外の表示

PROMPT=$PROMPT'${vcs_info_msg_0_} '
