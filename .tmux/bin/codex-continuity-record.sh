#!/usr/bin/env bash
# Codex の hook（SessionStart・UserPromptSubmit）から呼ばれ、tmux-claude-continuity が
# 復元時に再実行するペインの起動記録を「codex resume <session_id> …」に書き換える。
# 起動記録は zsh の preexec が入力行をそのまま書いたもので、復元後のシェルが eval する。
#
# 誤った会話を resume するより、シェルで戻るほうが害が小さい。
# そのため、TMUX_PANE のペインで Codex が動いていると確かめられないときは何も書かない。
# 例: Claude がツールとして実行した codex exec が、Claude ペインの起動記録を上書きしないようにする。
#
# hook はペインの Codex ではなくバックグラウンドの app-server daemon から起動されるため、
# プロセスの親子関係ではペインと結び付けられない。ペインの前面プロセスと作業ディレクトリで確かめる。
set -u

readonly CODEX_COMMAND=codex
# -c/--config の値に秘密情報が入っていたら、平文の起動記録に残さない
readonly SECRET_CONFIG_KEY_PATTERN='(token|secret|password|api_key|apikey)'

hook_input="$(cat)"
pane="${TMUX_PANE:-}"
[ -n "$pane" ] || exit 0

session_id="$(printf '%s' "$hook_input" | jq -r '.session_id // empty' 2>/dev/null)"
# 起動記録は eval されるため、ID は英数字と区切り文字だけを受け付ける
[[ "$session_id" =~ ^[A-Za-z0-9_.-]+$ ]] || exit 0
session_cwd="$(printf '%s' "$hook_input" | jq -r '.cwd // empty' 2>/dev/null)"

pane_info="$(tmux display-message -p -t "$pane" '#{pane_current_command}	#{pane_current_path}' 2>/dev/null)" || exit 0
pane_command="${pane_info%%	*}"
pane_path="${pane_info#*	}"
[ "$pane_command" = "$CODEX_COMMAND" ] || exit 0
# daemon から渡る TMUX_PANE が別の Codex ペインを指していても、作業ディレクトリが違えば気づける
[ -n "$session_cwd" ] && [ "$session_cwd" = "$pane_path" ] || exit 0

launch_dir="$(tmux show-option -gqv @claude-continuity-launch-dir 2>/dev/null)"
launch_dir="${launch_dir:-$HOME/.config/tmux-claude/launch}"
launch_file="${launch_dir}/${pane#%}"

# 入力行を zsh と同じ規則で単語に分ける（クォートを外し、展開はしない）
split_shell_words() {
  zsh -fc 'print -rl -- "${(Q@)${(z)1}}"' _ "$1"
}

# 入力行から、resume にも引き継げるオプションだけを取り出す。
# 値を取るかどうか分からないオプションがあると後続の単語を正しく読めないため、
# そのときは引き継ぎを諦めて素の resume にする。
carried_options=()
collect_carried_options() {
  local words=() word option value
  [ -s "$launch_file" ] || return 0
  while IFS= read -r word; do words+=("$word"); done < <(split_shell_words "$(head -n 1 "$launch_file")")
  [ "${#words[@]}" -gt 0 ] && [ "${words[0]}" = "$CODEX_COMMAND" ] || return 0

  local i=1 options=()
  while [ "$i" -lt "${#words[@]}" ]; do
    word="${words[$i]}"
    i=$((i + 1))
    case "$word" in
      --) break ;;
      --*=*) option="${word%%=*}"; value="${word#*=}" ;;
      -*) option="$word"; value="" ;;
      *)
        # 位置引数は resume・fork と旧セッション ID、または初回のプロンプトで、どれも引き継がない
        continue
        ;;
    esac

    case "$option" in
      -m | --model | -p | --profile | -s | --sandbox | -a | --ask-for-approval | -C | --cd | \
        --add-dir | --enable | --disable | --local-provider | --remote | --remote-auth-token-env | \
        -c | --config)
        if [ -z "$value" ]; then
          [ "$i" -lt "${#words[@]}" ] || return 0
          value="${words[$i]}"
          i=$((i + 1))
        fi
        case "$option" in
          -c | --config)
            [[ "${value%%=*}" =~ $SECRET_CONFIG_KEY_PATTERN ]] && continue
            # 短縮形の -c は continuity が Claude の --continue と見なして取り除くため、長い形で残す
            option=--config
            ;;
        esac
        options+=("$option" "$value")
        ;;
      --oss | --strict-config | --approve-for-me | --dangerously-bypass-approvals-and-sandbox | \
        --dangerously-bypass-hook-trust | --search | --no-alt-screen | --no-daemon)
        options+=("$option")
        ;;
      --last | --all | --include-non-interactive | --worktree)
        # 会話の選び方や新しい worktree の作成は、ID で resume するときには不要
        ;;
      *)
        return 0
        ;;
    esac
  done
  carried_options=("${options[@]+"${options[@]}"}")
}
collect_carried_options

resume_command="$(printf '%q ' "$CODEX_COMMAND" resume "$session_id" "${carried_options[@]+"${carried_options[@]}"}")"
resume_command="${resume_command% }"

umask 077
mkdir -p "$launch_dir" || exit 0
tmp_file="$(mktemp "${launch_dir}/.codex-record.XXXXXX")" || exit 0
printf '%s\n' "$resume_command" > "$tmp_file" && mv -f "$tmp_file" "$launch_file"
rm -f "$tmp_file"
