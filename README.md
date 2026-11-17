# shell-configs

macOS で使っているシェル・ターミナル・エディタの設定と、Homebrew のパッケージ一覧です。
リポジトリの中の配置は、ホームディレクトリの配置と同じです（例: `.config/starship.toml` → `~/.config/starship.toml`）。

## 管理しているもの

| 分類            | ファイル                                                   | 内容                                                                          |
| --------------- | ---------------------------------------------------------- | ----------------------------------------------------------------------------- |
| zsh             | `.zshenv`、`.zprofile`、`.zshrc`                           | シェルの設定                                                                  |
| zsh             | `.config/sheldon/plugins.toml`                             | [sheldon](https://github.com/rossmacarthur/sheldon) で入れる zsh のプラグイン |
| zsh             | `.config/starship.toml`                                    | [Starship](https://starship.rs/) のプロンプト                                 |
| tmux            | `.tmux.conf`                                               | tmux の設定                                                                   |
| tmux            | `.tmux/bin/`                                               | ステータスバーの表示と、AI エージェントの状態表示に使うスクリプト             |
| Ghostty         | `Library/Application Support/com.mitchellh.ghostty/config` | [Ghostty](https://ghostty.org/) の設定                                        |
| Ghostty         | `.config/ghostty/themes/`                                  | Ghostty の自作テーマ                                                          |
| Emacs           | `.emacs.d/`                                                | Emacs の設定と操作の手引き                                                    |
| AI エージェント | `.claude/tmux-hooks.json`                                  | Claude Code の hook のうち、tmux 連携に使うもの                               |
| AI エージェント | `.codex/hooks.json`                                        | Codex の hook                                                                 |
| Homebrew        | `Brewfile.*`                                               | 端末ごとのパッケージ一覧                                                      |
| Homebrew        | `brewrenewal.sh`                                           | Homebrew のパッケージをまとめて更新するスクリプト                             |
| iTerm2          | `iterm2-profile.json`                                      | iTerm2 のプロファイル（今は Ghostty を使っているため更新していません）        |

## 管理していないもの

- `~/.zshrc.local`: 会社の設定など、公開できないものを置きます。`.zshrc` は、このファイルがあるときだけ読み込みます。
- `~/.claude/settings.json`: hook 以外の個人設定も多く含むため、tmux 連携の hook だけを `.claude/tmux-hooks.json` に分けています。
- `~/.tmux/plugins/`: 次の「tmux」の手順で、別に入れます。

## セットアップ

### Homebrew

端末に合った Brewfile から、パッケージを入れます。

```sh
brew bundle --file=Brewfile.business
```

いまの端末の状態を Brewfile に書き出すときは、次を実行します。

```sh
brew bundle dump --file=Brewfile.business --force
```

### 設定ファイル

リポジトリの中のファイルを、ホームディレクトリの同じ場所にコピーするか、シンボリックリンクを張ります。

```sh
ln -s ~/GitHub/Personal/shell-configs/.zshrc ~/.zshrc
```

### zsh

sheldon でプラグインを入れます。

```sh
sheldon lock
```

### tmux

プラグインは TPM を使わず、`~/.tmux/plugins/` に手で clone しています。

```sh
git clone https://github.com/tmux-plugins/tmux-resurrect ~/.tmux/plugins/tmux-resurrect
git clone https://github.com/tmux-plugins/tmux-continuum ~/.tmux/plugins/tmux-continuum
git clone https://github.com/MadAppGang/tmux-claude-continuity ~/.tmux/plugins/tmux-claude-continuity
```

### Claude Code

`.claude/tmux-hooks.json` の hook を、`~/.claude/settings.json` に足します。
すでにある hook は残し、同じイベントには後ろに追加します。

```sh
jq -s '.[1].hooks as $add | .[0] | .hooks = reduce ($add | to_entries[]) as $e (.hooks // {}; .[$e.key] += $e.value)' \
  ~/.claude/settings.json .claude/tmux-hooks.json > ~/.claude/settings.json.new \
  && mv ~/.claude/settings.json.new ~/.claude/settings.json
```

### Codex

`.codex/hooks.json` を `~/.codex/hooks.json` に置きます。
Codex の hook は、スクリプトを絶対パス（`/Users/sagari/...`）で指定しています。ユーザー名が違う端末では、パスを書き換えてください。
置いたあとに Codex で `/hooks` を開き、各 hook を承認します。

## tmux と AI エージェントの連携

Claude Code と Codex を tmux で動かすと、次のように連携します。

- 各エージェントの hook が `claude-agent-state.sh` を呼び、ウィンドウ名に状態を出します（✋ 入力待ち、⏳ 作業中、✅ 完了）。
- `prefix + a` でエージェントの一覧を開きます。`prefix + j` で、次の対応待ちのペインに移動します。
- 端末を再起動して tmux を復元すると、各ペインの会話が再開します。
  - Claude Code は tmux-claude-continuity が `claude --resume` で再開します。
  - Codex は `codex-continuity-record.sh` が起動記録を `codex resume <id>` に書き換え、同じ仕組みで再開します。
