;;; init.el --- Personal Emacs configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;; パッケージ管理は leaf に統一しています。
;;
;; 構成の並び順:
;;   1. leaf ブートストラップ
;;   2. 基本設定（ユーザー情報・言語・エディタ挙動）
;;   3. 編集支援（括弧・インデント・行番号）
;;   4. 補完（company / ivy 系）
;;   5. 外部ツール連携（magit / flyspell）
;;   6. 外観とウィンドウ（テーマ・popwin・dirvish）
;;   7. 言語別設定
;;   8. LLM 連携（gptel）
;;
;; 秘密情報は init.el に書かず、auth-source（~/.authinfo.gpg）から読みます。
;; Customize が自動生成する設定は custom.el に隔離し、本ファイルは手書き設定のみを持ちます。

;;; Code:

;;;; 1. leaf ブートストラップ

;; <leaf-install-code>
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org"   . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu"   . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    ;; el-get は読み込み時に "Package autoload is deprecated" を出すうえ
    ;; :el-get キーワードを使っていないため入れない
    (leaf hydra :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))
;; </leaf-install-code>

(leaf leaf
  :config
  (leaf leaf-convert :ensure t)
  (leaf leaf-tree
    :doc "leaf ブロックをサイドバーでツリー表示する"
    :ensure t
    :custom ((imenu-list-size . 30)
             (imenu-list-position . 'left))))


;;;; 2. 基本設定

(leaf *user-settings
  :setq ((user-full-name . "Takuma Katanosaka")
         (user-mail-address . "bumpbumpbump.v@gmail.com")))

(leaf *language-settings
  :config
  (set-language-environment 'Japanese)
  (prefer-coding-system 'utf-8))

(leaf *editor-settings
  :doc "組み込み変数のみを扱う。パッケージ固有の設定は各 leaf ブロックへ置く"
  :custom ((confirm-kill-emacs . 'y-or-n-p)
           ;; yes/no の入力を y/n で済ませる
           (use-short-answers . t)
           ;; バックアップと自動保存の生成物は残さない
           (backup-inhibited . t)
           (delete-auto-save-files . t)
           ;; 削除をゴミ箱経由にして取り返しをつける
           (delete-by-moving-to-trash . t)
           (completion-ignore-case . t)
           (scroll-conservatively . 1)
           ;; 外部変更の取り込みは黙って行う
           (auto-revert-verbose . nil)
           (global-auto-revert-non-file-buffers . t))
  :setq-default ((tab-width . 2)
                 (indent-tabs-mode . nil))
  :config
  (global-hl-line-mode 1)
  ;; 選択範囲がある状態で入力したら置き換える
  (delete-selection-mode 1)
  ;; camelCase を単語単位で移動する
  (global-subword-mode 1)
  ;; minified ファイルなど極端に長い行で固まらせない
  (global-so-long-mode 1)
  ;; git 操作で変わったファイルを自動で読み直す
  (global-auto-revert-mode 1))

(leaf subword
  :doc "global-subword-mode のモードライン表示を隠す"
  :tag "builtin"
  :blackout subword-mode)

(leaf server
  :doc "emacsclient から既存の Emacs へファイルを送れるようにする"
  :tag "builtin"
  :require t
  :defun server-running-p
  :config
  ;; 権限やソケットの残骸で失敗しても起動全体を止めない。
  ;; 失敗を黙って捨てると原因が追えないため message には残す
  (unless (server-running-p)
    (condition-case err
        (server-start)
      (error (message "server-start に失敗しました: %s"
                      (error-message-string err))))))

(leaf display-line-numbers
  :doc "行番号表示（Emacs 26 以降の組み込み。nlinum の後継）"
  :tag "builtin"
  :hook (prog-mode-hook . display-line-numbers-mode))

(leaf recentf
  :doc "最近開いたファイルの履歴。C-x C-r の counsel-recentf が参照する"
  :tag "builtin"
  :custom `((recentf-save-file . ,(expand-file-name "var/recentf" user-emacs-directory))
            (recentf-max-saved-items . 500)
            ;; 起動時に存在確認で待たされないよう自動掃除を切る
            (recentf-auto-cleanup . 'never)
            (recentf-exclude . '("/elpa/" "/var/" "\\.gpg\\'")))
  :global-minor-mode recentf-mode)

(leaf savehist
  :doc "ミニバッファ履歴を再起動後も保持する"
  :tag "builtin"
  :custom `((savehist-file . ,(expand-file-name "var/history" user-emacs-directory)))
  :global-minor-mode savehist-mode)

(leaf saveplace
  :doc "ファイルごとにカーソル位置を復元する"
  :tag "builtin"
  :custom `((save-place-file . ,(expand-file-name "var/places" user-emacs-directory)))
  :global-minor-mode save-place-mode)

(leaf which-key
  :doc "打ちかけのキーに続く候補を表示する（Emacs 30 の組み込み）"
  :tag "builtin"
  :blackout t
  :custom ((which-key-idle-delay . 0.7))
  :global-minor-mode which-key-mode)


;;;; 3. 編集支援

(leaf rainbow-delimiters
  :doc "対応する括弧を階層ごとに色分けする"
  :ensure t
  :hook (prog-mode-hook . rainbow-delimiters-mode))

(leaf paren
  :doc "対応する括弧をハイライトする"
  :tag "builtin"
  :custom ((show-paren-delay . 0.1)
           (show-paren-style . 'mixed))
  :custom-face ((show-paren-match quote ((nil (:background unspecified :underline "turquoise")))))
  :global-minor-mode show-paren-mode)

(leaf smartparens
  :doc "括弧の対応を自動補完する"
  :ensure t
  :require smartparens-config
  :custom ((electric-pair-mode . nil))
  :hook (after-init-hook . smartparens-global-mode))

(leaf highlight-indent-guides
  :doc "インデントを縦線でハイライトする"
  :ensure t
  :blackout t
  :custom ((highlight-indent-guides-method . 'character)
           (highlight-indent-guides-auto-enabled . nil)
           (highlight-indent-guides-responsive . t)
           (highlight-indent-guides-character . ?|))
  :hook (prog-mode-hook . highlight-indent-guides-mode))


(leaf avy
  :doc "画面内の任意の位置へ数打鍵でジャンプする"
  :ensure t
  :custom ((avy-timeout-seconds . 0.4))
  ;; C-: と C-' は端末で Ctrl が落ちるため goto 系プレフィクスへ寄せる。
  ;; avy-goto-line は行番号の直接入力も受けるので goto-line の上位互換になる
  :bind (("M-g w" . avy-goto-char-timer)
         ("M-g g" . avy-goto-line)))

(leaf expreg
  :doc "選択範囲を構文単位で広げる。tree-sitter を利用する"
  :ensure t
  ;; C-= と C-M-= は端末で Ctrl が落ちるため C-c 配下へ移す
  :bind (("C-c =" . expreg-expand)
         ("C-c -" . expreg-contract)))

(leaf undo-fu
  :doc "undo と redo を直感的な挙動にする"
  :ensure t
  ;; 端末は C-/ を C-_ として送るため C-_ に割り当てる。
  ;; C-? は DEL（Backspace）と同一になるので M-_ を使う
  :bind (("C-_" . undo-fu-only-undo)
         ("C-/" . undo-fu-only-undo)
         ("M-_" . undo-fu-only-redo)))

(leaf undo-fu-session
  :doc "undo 履歴をファイル単位で永続化し、再起動後も遡れるようにする"
  :ensure t
  ;; undo-fu には依存しない。:after を付けると undo-fu が遅延読み込みのため発火しない
  :custom `((undo-fu-session-directory
             . ,(expand-file-name "var/undo-fu-session" user-emacs-directory)))
  :global-minor-mode undo-fu-session-global-mode)

(leaf vundo
  :doc "undo 履歴を木構造で表示する"
  :ensure t
  :custom ((vundo-glyph-alist . vundo-unicode-symbols))
  :bind (("C-x u" . vundo)))

(leaf symbol-overlay
  :doc "カーソル位置と同じシンボルを強調し、その間を移動できるようにする"
  :ensure t
  :blackout t
  :bind (("M-i" . symbol-overlay-put))
  :hook (prog-mode-hook . symbol-overlay-mode))

(leaf string-inflection
  :doc "snake_case と CamelCase などを相互変換する"
  :ensure t
  :bind (("C-c i" . string-inflection-all-cycle)))

(leaf tempel
  :doc "軽量なテンプレート展開"
  :ensure t
  :custom `((tempel-path . ,(expand-file-name "templates" user-emacs-directory)))
  :bind (("M-+" . tempel-complete)
         ("M-*" . tempel-insert)))

(leaf breadcrumb
  :doc "ヘッダ行に現在位置のパンくずを表示する"
  :ensure t
  :hook (prog-mode-hook . breadcrumb-local-mode))

(leaf rg
  :doc "ripgrep による検索。rg コマンドは導入済み"
  :ensure t
  :custom ((rg-group-result . t))
  :bind (("C-c s" . rg-menu)))


;;;; 4. 補完

(leaf company
  :doc "オートコンプリート"
  :url "http://company-mode.github.io/"
  :emacs>= 24.3
  :ensure t
  :blackout t
  :leaf-defer nil
  :custom ((company-idle-delay . 0)
           (company-minimum-prefix-length . 2)
           (company-selection-wrap-around . t))
  :global-minor-mode global-company-mode)

(leaf ivy
  :doc "Incremental Vertical completYon"
  :url "https://github.com/abo-abo/swiper"
  :emacs>= 24.5
  :ensure t
  :blackout t
  :leaf-defer nil
  :custom ((ivy-initial-inputs-alist . nil)
           (ivy-use-selectable-prompt . t))
  :global-minor-mode t
  :config
  (leaf swiper
    :doc "Isearch with an overview. Oh, man!"
    :ensure t
    :bind (("C-s" . swiper)))

  (leaf counsel
    :doc "Various completion functions using Ivy"
    :ensure t
    :blackout t
    :custom `((counsel-yank-pop-separator . "\n----------\n")
              (counsel-find-file-ignore-regexp . ,(rx-to-string '(or "./" "../") 'no-group)))
    ;; C-S-s は端末で Shift が落ちて C-s（swiper）になるため M-s i へ。
    ;; M-s i は Emacs 標準の imenu と同じ位置づけ
    :bind (("M-s i" . counsel-imenu)
           ("C-x C-r" . counsel-recentf))
    :global-minor-mode t))

(leaf prescient
  :doc "履歴と頻度で補完候補を並べ替える"
  :url "https://github.com/raxod502/prescient.el"
  :emacs>= 25.1
  :ensure t
  :custom ((prescient-aggressive-file-save . t))
  :global-minor-mode prescient-persist-mode
  :config
  (leaf ivy-prescient
    :doc "prescient.el + Ivy"
    :ensure t
    :after ivy
    :custom ((ivy-prescient-retain-classic-highlighting . t))
    :global-minor-mode t))


;;;; 5. 外部ツール連携

(leaf magit
  :doc "Git フロントエンド"
  :ensure t
  :custom ((magit-completing-read-function . 'magit-builtin-completing-read)
           (magit-refs-show-commit-count   . 'all)
           (magit-log-buffer-file-locked   . t)
           (magit-revision-show-gravatars  . nil))
  :bind (("C-x g" . magit-status))
  :init
  (leaf transient
    :custom ((transient-force-fixed-pitch . t))))

(leaf diff-hl
  :doc "変更行を表示する。端末にはフリンジが無いため余白へ描く"
  :ensure t
  :blackout t
  :defun ((diff-hl-magit-pre-refresh . diff-hl)
          (diff-hl-magit-post-refresh . diff-hl))
  :global-minor-mode global-diff-hl-mode
  :config
  (unless (display-graphic-p)
    (diff-hl-margin-mode 1))
  ;; magit の操作後に表示を追従させる
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

(leaf difftastic
  :doc "構文を理解する diff。difft コマンドを利用する"
  :ensure t
  :global-minor-mode difftastic-bindings-mode)

(leaf flyspell
  :doc "スペルチェック"
  :tag "builtin"
  :if (executable-find "aspell")
  :blackout (flyspell-mode . "F")
  :custom ((ispell-program-name   . "aspell")
           ;; 言語環境が Japanese のままだと ja_JP 辞書を探して失敗するため明示する
           ;; 日本語そのものは ispell-skip-region-alist で照合対象外にする
           (ispell-dictionary      . "en_US")
           (ispell-check-comments . nil)
           (ispell-skip-html      . t)
           (ispell-silently-savep . t))
  :bind (:flyspell-mode-map
         ("C-." . nil)
         ("C-," . nil))
  :defer-config
  ;; 日本語は辞書照合の対象外にする
  (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))


;;;; 6. 外観とウィンドウ

(leaf modus-themes
  :doc "コントラスト重視の配色テーマ"
  :ensure t
  ;; modus-themes-load-theme は autoload されないため明示的に読み込む
  :require t
  :defun modus-themes-load-theme
  :config
  (modus-themes-load-theme 'modus-vivendi))

(leaf popwin
  :doc "ポップアップ用ウィンドウを一元管理する"
  :ensure t
  :blackout t
  :defvar popwin:special-display-config
  ;; direx 用の登録は撤去した。dirvish-side は dirvish-side-display-alist で
  ;; 自前にウィンドウを管理するため popwin 側の設定は不要
  :global-minor-mode popwin-mode)

(leaf ls-lisp
  :doc "ディレクトリを先頭に並べる。BSD ls は --group-directories-first 非対応のため
Lisp 実装の ls を使う"
  :tag "builtin"
  :require t
  :custom ((ls-lisp-use-insert-directory-program . nil)
           (ls-lisp-dirs-first . t)))

(leaf dired
  :doc "ファイル操作の土台。表示は dirvish が引き継ぐ"
  :tag "builtin"
  :custom (;; -h で人間可読なサイズにする
           (dired-listing-switches . "-alh")
           ;; 2 画面表示のときコピー先を隣のウィンドウから推測する
           (dired-dwim-target . t)
           ;; ディレクトリを辿るたびにバッファが増えるのを防ぐ
           (dired-kill-when-opening-new-dired-buffer . t)))

(leaf dirvish
  :doc "dired の上位互換。direx の後継としてツリー表示とプレビューを担う"
  :ensure t
  :custom (;; アイコン系の attribute は端末で意味がないため使わない
           (dirvish-attributes . '(vc-state subtree-state file-size collapse))
           (dirvish-side-width . 35)
           (dirvish-quick-access-entries
            . '(("h" "~/"                "home")
                ("e" "~/.emacs.d/"       "emacs.d")
                ("w" "~/GitHub/Work/"    "work"))))
  ;; dired 系のコマンドも dirvish の表示に寄せる
  :global-minor-mode dirvish-override-dired-mode
  :bind (("C-c d" . dirvish-dwim)
         ("C-c D" . dirvish-side)))


(leaf xref
  :doc "定義や参照へのジャンプ。VSCode の F12 相当を割り当てる"
  :tag "builtin"
  :custom ((xref-prompt-for-identifier . nil))
  :bind (("<f12>"   . xref-find-definitions)
         ("S-<f12>" . xref-find-references)
         ("M-<f12>" . xref-go-back)))


;;;; 7. 言語別設定

(leaf treesit-auto
  :doc "文法が導入済みの言語を *-ts-mode へ切り替える"
  :ensure t
  ;; autoload を一切持たないため明示的に読み込む
  ;; ファイルを開く前に auto-mode-alist を整える必要もある
  :require t
  :defun ((treesit-auto-add-to-auto-mode-alist . treesit-auto)
          (global-treesit-auto-mode . treesit-auto))
  :custom ((treesit-auto-langs . '(javascript typescript tsx json python ruby
                                   dockerfile yaml go gomod rust bash))
           ;; 未導入の文法を無断で取得しないよう確認を挟む
           (treesit-auto-install . 'prompt))
  :config
  ;; 素の Emacs は .ts .tsx .go .rs Dockerfile go.mod にモードを割り当てない
  (treesit-auto-add-to-auto-mode-alist 'all)
  ;; .jsx は Emacs 既定の javascript-mode 経由で js-ts-mode に解決される
  ;; JavaScript の文法は JSX を含むため追加の割り当ては不要
  (global-treesit-auto-mode))

(leaf web-mode
  :doc "複数言語が混在するテンプレート専用。単一言語ファイルは *-ts-mode に任せる"
  :ensure t
  :mode ("\\.html?\\'" "\\.erb\\'" "\\.rhtml\\'" "\\.ejs\\'"
         "\\.vue\\'" "\\.svelte\\'" "\\.astro\\'"
         "\\.hbs\\'" "\\.handlebars\\'"
         "\\.jinja2?\\'" "\\.j2\\'"))

(leaf terraform-mode
  :doc "Terraform / HCL。tree-sitter 文法が treesit-auto に無いため通常のモードを使う"
  :ensure t
  :custom ((terraform-indent-level . 2)
           ;; terraform コマンドが tfenv の shim で、バージョンが未導入のため無効。
           ;; `tfenv install' か `mise use -g terraform' の後に t へ変えると
           ;; 保存時に terraform fmt がかかる。
           (terraform-format-on-save . nil))
  :mode ("\\.tf\\'" "\\.tfvars\\'"))

(leaf markdown-mode
  :doc "Markdown。markdown-ts-mode は Emacs 31 以降のため本体を使う"
  :ensure t
  :custom ((markdown-fontify-code-blocks-natively . t)
           (markdown-indent-on-enter . 'indent-and-new-item))
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode)))

(leaf sql-indent
  :doc "SQL のインデントを構文に沿って揃える"
  :ensure t
  :blackout sqlind-minor-mode
  :hook (sql-mode-hook . sqlind-minor-mode))

(leaf sqlformat
  :doc "SQL の整形。pg_format を利用する"
  :ensure t
  :custom ((sqlformat-command . 'pgformatter)
           ;; インデント 2 桁、GROUP BY などのキーワードを整える
           (sqlformat-args . '("-s2" "-g")))
  ;; 既存の .sql は pg_format 出力と差があるため保存時整形はしない。
  ;; 自動化するなら (sql-mode-hook . sqlformat-on-save-mode) を足す。
  ;; sql-mode-map は sql（組み込み）が持つため、待つ対象を明示する
  :bind ((:sql-mode-map :package sql
                        ("C-c C-f" . sqlformat-buffer))))

(defvar my:eglot-server-executables
  '((js-ts-mode         "typescript-language-server")
    (typescript-ts-mode "typescript-language-server")
    (tsx-ts-mode        "typescript-language-server")
    (python-ts-mode     "pylsp" "basedpyright-langserver" "pyright-langserver")
    (ruby-ts-mode       "ruby-lsp" "solargraph")
    ;; Rust は除外。mise の rust-analyzer は toolchain 未導入の rustup
    ;; プロキシで、実行可能に見えて起動に失敗するため
    (go-ts-mode         "gopls")
    (bash-ts-mode       "bash-language-server")
    (yaml-ts-mode       "yaml-language-server")
    (json-ts-mode       "vscode-json-language-server")
    (dockerfile-ts-mode "docker-langserver")
    (terraform-mode     "terraform-ls"))
  "メジャーモードごとの言語サーバー実行ファイル候補。")

(defun my:eglot-ensure-if-server-installed ()
  "言語サーバーが導入済みのときだけ eglot を起動する。

サーバーが無い状態で `eglot-ensure' を呼ぶと、ファイルを開くたびに
警告が出る。実行ファイルの有無で判定することで、サーバーを後から
入れた場合も設定を書き換えずに有効になる。"
  (when (seq-some #'executable-find
                  (alist-get major-mode my:eglot-server-executables))
    (eglot-ensure)))

(leaf eglot
  :doc "LSP クライアント。F12 のジャンプを言語サーバー経由で提供する"
  :tag "builtin"
  :defvar eglot-server-programs
  :hook ((js-ts-mode-hook typescript-ts-mode-hook tsx-ts-mode-hook
          python-ts-mode-hook ruby-ts-mode-hook go-ts-mode-hook
          rust-ts-mode-hook bash-ts-mode-hook yaml-ts-mode-hook
          json-ts-mode-hook dockerfile-ts-mode-hook
          terraform-mode-hook)
         . my:eglot-ensure-if-server-installed)
  ;; VSCode 相当の操作。F2 は既定で 2C-command（二段組）に占有されているため
  ;; eglot-mode-map に限定して上書きし、LSP 以外のバッファでは元の意味を残す。
  ;; コードアクションと整形は C-. と S-M-f が端末で届かないため C-c 配下に置く
  :bind ((:eglot-mode-map :package eglot
                          ("<f2>" . eglot-rename)
                          ("C-c a" . eglot-code-actions)
                          ("C-c f" . eglot-format-buffer)))
  :defer-config
  ;; 実行可能な候補が複数あると eglot は毎回どれを使うか尋ねてくる。
  ;; basedpyright は pyright 互換の実行ファイルを同梱し、ruby は gem に
  ;; typeprof が付くため、いずれも候補が 2 つになる。使うものを固定する。
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode)
                 . ("basedpyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               '((ruby-mode ruby-ts-mode) . ("ruby-lsp"))))


;;;; Markdown の整形と校正

(defun my:node-local-executable (name)
  "NAME を node_modules/.bin から優先して探す。

textlint の設定は prh.yml などを相対パスで参照するため、プロジェクト付属の
実行ファイルでないと解決できない。見つからない場合は PATH 上のものへ落とす。"
  (let* ((start (or (and (buffer-file-name)
                         (file-name-directory (buffer-file-name)))
                    default-directory))
         (rel (concat "node_modules/.bin/" name))
         (dir (locate-dominating-file
               start
               (lambda (d) (file-executable-p (expand-file-name rel d))))))
    (if dir
        (expand-file-name rel dir)
      (executable-find name))))

(leaf apheleia
  :doc "保存時の整形。非同期で走り、カーソルとスクロール位置を保つ"
  :ensure t
  :blackout t
  :defvar (apheleia-formatters apheleia-mode-alist)
  ;; 全言語へ広げると意図しない差分が出るため markdown だけに絞る
  :hook ((markdown-mode-hook gfm-mode-hook) . apheleia-mode)
  :defer-config
  ;; textlint --fix は fixed-result 形式で修正後の全文を標準出力へ返す。
  ;; 修正対象が無い場合も全文を返すため、連鎖させてもバッファを失わない。
  ;;
  ;; --dry-run が必須。これが無いと textlint は --stdin-filename の実ファイルを
  ;; 直接書き換えてしまい、Emacs が「changed on disk」を尋ねて連鎖が中断する。
  (setf (alist-get 'textlint-fix apheleia-formatters)
        '("apheleia-npx" "textlint" "--stdin" "--stdin-filename" filepath
          "--fix" "--dry-run" "--format" "fixed-result"))
  ;; 文章を直す textlint を先に、体裁を整える prettier を後に走らせる
  (dolist (mode '(markdown-mode gfm-mode))
    (setf (alist-get mode apheleia-mode-alist) '(textlint-fix prettier-markdown))))

(leaf flymake
  :doc "診断の土台。eglot も同じ仕組みを使う"
  :tag "builtin"
  :custom (;; 端末にはフリンジが無いため余白へ印を出す
           (flymake-indicator-type . 'margins)
           (flymake-no-changes-timeout . 1.0))
  ;; VSCode の F8 相当。診断一覧は C-c ! で開く
  :bind (("<f8>" . flymake-goto-next-error)
         ("M-<f8>" . flymake-goto-prev-error)
         ("C-c !" . flymake-show-project-diagnostics)))

(leaf flymake-collection
  :doc "textlint 用チェッカーを定義するためのマクロを借りる"
  :ensure t
  ;; マクロが展開する内部関数（flymake--log-1）は実行時にも flymake が必要
  :require flymake flymake-collection flymake-collection-define)

;; マクロ展開はコンパイル時に行う。leaf の :require は実行時のみのため別に宣言する
(eval-when-compile
  (require 'flymake)
  (require 'flymake-collection-define nil t))

;; マクロ展開で flymake の内部関数を呼ぶ。leaf の :require は静的解析から
;; 見えないため、宣言だけ別に置く
(declare-function flymake--log-1 "flymake")

(flymake-collection-define-rx my:flymake-textlint
  "Markdown の文章校正を textlint で行う。"
  :title "textlint"
  :pre-let ((textlint-exec (my:node-local-executable "textlint")))
  :pre-check (unless textlint-exec
               (error "textlint の実行ファイルが見つかりません"))
  :write-type 'pipe
  :command `(,textlint-exec
             "--stdin"
             "--stdin-filename"
             ,(or (buffer-file-name flymake-collection-source) "stdin.md")
             "--format" "unix")
  :regexps
  ((error bol (file-name) ":" line ":" column ": " (message)
          " [Error/" (id (+ (not (any "]")))) "]" eol)
   (warning bol (file-name) ":" line ":" column ": " (message)
            " [Warning/" (id (+ (not (any "]")))) "]" eol)))

(defun my:markdown-textlint-setup ()
  "textlint が使えるときだけ Markdown の校正を有効にする。

--fix で直せない指摘（一文の長さ、助詞の重複など）を表示する役割。"
  (when (my:node-local-executable "textlint")
    (add-hook 'flymake-diagnostic-functions #'my:flymake-textlint nil t)
    (flymake-mode 1)))

(add-hook 'markdown-mode-hook #'my:markdown-textlint-setup)
(add-hook 'gfm-mode-hook #'my:markdown-textlint-setup)


;;;; ブラウザ連携

;; chawan（cha）は端末で単体運用し、Emacs からは URL を投げるだけにする。
;; w3m と違い CSS と JavaScript を解釈するため、素の eww より読める範囲が広い。
(defun my:browse-url-chawan (url &optional _new-window)
  "URL を chawan で開く。

tmux 内なら新しいウィンドウを起こして独立した端末を与える。tmux の外では
Emacs の term バッファで動かす。chawan が無い環境では eww へ落とすので、
未導入のまま設定を読んでも壊れない。"
  (interactive (browse-url-interactive-arg "URL: "))
  (let ((cha (executable-find "cha")))
    (cond
     ((null cha)
      (message "cha が見つからないため eww で開きます")
      (eww-browse-url url))
     ((getenv "TMUX")
      ;; 0 を渡して非同期にする。Emacs は待たない
      (call-process "tmux" nil 0 nil "new-window" "-n" "cha" cha url))
     (t
      (browse-url-text-emacs url)))))

(leaf browse-url
  :doc "URL の開き先。既定は chawan、JavaScript が必要な場面は OS のブラウザへ"
  :tag "builtin"
  :custom ((browse-url-browser-function . 'my:browse-url-chawan)
           ;; C-u 付きの呼び出しと eww の & がこちらを使う
           (browse-url-secondary-browser-function . 'browse-url-default-browser)
           ;; tmux の外に出たとき browse-url-text-emacs が使う実行ファイル
           (browse-url-text-browser . "cha"))
  :bind (("C-c u" . browse-url-at-point)
         ("C-c U" . browse-url)))

(leaf eww
  :doc "Emacs 内で完結させたいときのブラウザ。chawan とは用途で使い分ける"
  :tag "builtin"
  :custom ((eww-search-prefix . "https://duckduckgo.com/html/?q=")
           ;; 履歴とブックマークを var/ へ集約する
           (eww-bookmarks-directory . "~/.emacs.d/var/"))
  :bind (("C-c w" . eww)))


;;;; 8. LLM 連携

(leaf gptel
  :doc "Interact with ChatGPT or other LLMs"
  :url "https://github.com/karthink/gptel"
  :emacs>= 27.1
  :ensure t
  :defvar gptel-backend
  :defun (gptel-api-key-from-auth-source . gptel-request)
  :custom ((gptel-model . 'gemini-pro-latest))
  :config
  ;; API キーは auth-source から読む。~/.authinfo.gpg に次の 1 行を置く:
  ;;   machine generativelanguage.googleapis.com login apikey password <API_KEY>
  (setq gptel-backend
        (gptel-make-gemini "Gemini"
          :key #'gptel-api-key-from-auth-source
          :stream t)))


;;;; init.el 自身の再コンパイル

;; パッケージ設定ではないため leaf ブロックにしない
(defun my:byte-compile-init-file ()
  "init.el を保存したら init.elc を作り直す。

`load-prefer-newer' が nil の環境では、init.el を編集しても古い init.elc が
優先されるため、編集が黙って無視される。コンパイルに失敗したときは init.elc を
削除し、次回起動でソースが読まれる状態に戻す。"
  (when (equal (buffer-file-name)
               (expand-file-name "init.el" user-emacs-directory))
    ;; 保存のたびに *Compile-Log* を開かせない
    (let ((display-buffer-alist
           (cons '("\\*Compile-Log\\*"
                   (display-buffer-no-window) (allow-no-window . t))
                 display-buffer-alist)))
      (if (byte-compile-file (buffer-file-name))
          (message "init.elc を更新しました")
        (let ((elc (concat (buffer-file-name) "c")))
          (when (file-exists-p elc)
            (delete-file elc)))
        (message "init.el のコンパイルに失敗しました。init.elc を削除しました")))))

(add-hook 'after-save-hook #'my:byte-compile-init-file)


;;;; Customize 用ファイル

;; Customize の自動生成分を init.el に混ぜず、custom.el に分離する
(customize-set-variable 'custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

(provide 'init)

;;; init.el ends here
