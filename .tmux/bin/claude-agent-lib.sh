# shellcheck shell=bash
# claude-agent-state.sh と claude-agent-switch.sh が共有する、状態の優先順位と表示記号。
# 複数の状態をまとめて1つに表すときは、人の対応が必要なものほど優先する。

readonly CLAUDE_STATES_BY_URGENCY="blocked done working idle"

# 標準入力の状態一覧（1行1状態）から、最も急ぐ状態を返す
most_urgent_state() {
  local states state
  states="$(cat)"
  for state in $CLAUDE_STATES_BY_URGENCY; do
    if printf '%s\n' "$states" | grep -qx "$state"; then
      printf '%s' "$state"
      return
    fi
  done
}

urgency_rank() {
  case "$1" in
    blocked) echo 0 ;;
    done) echo 1 ;;
    working) echo 2 ;;
    idle) echo 3 ;;
    *) echo 9 ;;
  esac
}

state_icon() {
  case "$1" in
    blocked) printf '✋' ;;
    done) printf '✅' ;;
    working) printf '⏳' ;;
    idle) printf '💤' ;;
    *) printf '  ' ;;
  esac
}
