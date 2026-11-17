#!/usr/bin/env bash
# herdr のサイドバーの代わりに、エージェントやセッションを状態つきで一覧し、選んだ先へ移動する。
#
# 使い方: claude-agent-switch.sh <agents|sessions|next [現在のpane_id]>
#   agents   Claude Code・Codex が動くペインを急ぐ順に fzf で選ぶ
#   sessions セッションを、中のエージェントの集約状態つきで fzf で選ぶ
#   next     対応待ち（blocked → done）のペインへ fzf を使わずに移動する
set -u

# shellcheck source=SCRIPTDIR/claude-agent-lib.sh
. "$(dirname "$0")/claude-agent-lib.sh"

readonly TAB=$'\t'
readonly PREVIEW_LINES=60

jump_to_pane() {
  tmux select-window -t "$1" \; select-pane -t "$1" \; switch-client -t "$1"
}

require_fzf() {
  command -v fzf >/dev/null && return
  echo "fzf が見つかりません（brew install fzf）" >&2
  read -r -n 1 -s -p "何かキーを押すと閉じます"
  exit 1
}

# 出力: 優先度<TAB>pane_id<TAB>表示用の行
list_agents() {
  tmux list-panes -a -F "#{@claude_state}${TAB}#{pane_id}${TAB}#{session_name}:#{window_index}.#{pane_index}${TAB}#{window_name}${TAB}#{b:pane_current_path}" |
    while IFS="$TAB" read -r state pane location window_name dir; do
      [ -n "$state" ] || continue
      printf '%s\t%s\t%s %-8s %-16s %s\n' \
        "$(urgency_rank "$state")" "$pane" "$(state_icon "$state")" "$state" "$location" "$window_name ($dir)"
    done |
    sort -t "$TAB" -k1,1n
}

pick_agent() {
  local selected
  require_fzf
  selected="$(list_agents | cut -f2- |
    fzf --delimiter="$TAB" --with-nth=2 --reverse --prompt='agent> ' \
      --preview="tmux capture-pane -ep -t {1} | tail -n $PREVIEW_LINES" \
      --preview-window=down,60%)" || exit 0
  jump_to_pane "$(printf '%s' "$selected" | cut -f1)"
}

list_sessions() {
  local session_id session_name attached state
  tmux list-sessions -F "#{session_id}${TAB}#{session_name}${TAB}#{?session_attached,*, }" |
    while IFS="$TAB" read -r session_id session_name attached; do
      state="$(tmux list-panes -s -t "$session_id" -F '#{@claude_state}' | most_urgent_state)"
      printf '%s\t%s %s %s\n' "$session_id" "$attached" "$(state_icon "$state")" "$session_name"
    done
}

pick_session() {
  local selected
  require_fzf
  selected="$(list_sessions |
    fzf --delimiter="$TAB" --with-nth=2 --reverse --prompt='session> ' \
      --preview="tmux capture-pane -ep -t {1} | tail -n $PREVIEW_LINES" \
      --preview-window=down,60%)" || exit 0
  tmux switch-client -t "$(printf '%s' "$selected" | cut -f1)"
}

jump_to_next_attention() {
  local current="$1" target
  target="$(list_agents | awk -F "$TAB" -v current="$current" '$1 <= 1 && $2 != current { print $2; exit }')"
  if [ -z "$target" ]; then
    tmux display-message '対応待ちのエージェントはありません'
    return
  fi
  jump_to_pane "$target"
}

case "${1:-}" in
  agents) pick_agent ;;
  sessions) pick_session ;;
  next) jump_to_next_attention "${2:-$(tmux display-message -p '#{pane_id}')}" ;;
  *)
    echo "usage: $0 <agents|sessions|next>" >&2
    exit 2
    ;;
esac
