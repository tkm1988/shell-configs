#!/usr/bin/env bash
# Claude Code・Codex の hook と tmux の hook から呼ばれ、エージェントの状態を tmux のユーザーオプションに記録する。
# 状態はペインごとに持ち、ウィンドウとサーバー全体には集約した結果を置いてステータスバーから参照する。
# 状態ファイルを使わないのは、ペインが消えれば状態も一緒に消え、掃除が要らないため。
#
# 使い方: claude-agent-state.sh <working|blocked|done|clear|seen> [pane_id]
#   seen は tmux の hook 用で、見たペインの done を idle に戻す
set -u

# shellcheck source=SCRIPTDIR/claude-agent-lib.sh
. "$(dirname "$0")/claude-agent-lib.sh"

event="${1:-}"
pane="${2:-${TMUX_PANE:-}}"

# Claude Code と Codex は hook の入力を標準入力に JSON で渡す。使わないが、読み切って書き手を待たせない。
# tmux の run-shell から呼ばれる seen では標準入力が閉じる保証がないため読まない
if [ "$event" != seen ] && [ ! -t 0 ]; then
  cat >/dev/null
fi

# tmux の外で動くエージェントでは何もしない
[ -n "$pane" ] || exit 0
tmux display-message -p -t "$pane" '' >/dev/null 2>&1 || exit 0

is_viewed() {
  [ "$(tmux display-message -p -t "$1" '#{&&:#{pane_active},#{&&:#{window_active},#{session_attached}}}')" = 1 ]
}

refresh_rollups() {
  local window_state all_states blocked_count done_count summary=""

  window_state="$(tmux list-panes -t "$pane" -F '#{@claude_state}' | most_urgent_state)"
  tmux set-option -w -t "$pane" @claude_win_state "$window_state"

  all_states="$(tmux list-panes -a -F '#{@claude_state}')"
  blocked_count="$(printf '%s\n' "$all_states" | grep -cx blocked)"
  done_count="$(printf '%s\n' "$all_states" | grep -cx "done")"
  [ "$blocked_count" -gt 0 ] && summary="${summary}$(state_icon blocked)${blocked_count} "
  [ "$done_count" -gt 0 ] && summary="${summary}$(state_icon "done")${done_count} "
  tmux set-option -g @claude_summary "$summary"
}

case "$event" in
  working | blocked)
    tmux set-option -p -t "$pane" @claude_state "$event"
    ;;
  done)
    # 目の前で終わったものに未確認の印を付けても、消すためだけにペインを切り替える手間が増える
    if is_viewed "$pane"; then
      tmux set-option -p -t "$pane" @claude_state idle
    else
      tmux set-option -p -t "$pane" @claude_state "done"
    fi
    ;;
  seen)
    [ "$(tmux display-message -p -t "$pane" '#{@claude_state}')" = "done" ] || exit 0
    tmux set-option -p -t "$pane" @claude_state idle
    ;;
  clear)
    tmux set-option -p -t "$pane" -u @claude_state
    ;;
  *)
    echo "usage: $0 <working|blocked|done|clear|seen> [pane_id]" >&2
    exit 2
    ;;
esac

refresh_rollups
