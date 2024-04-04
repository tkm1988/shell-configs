;; <leaf-install-code>
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))
;; </leaf-install-code>

;; General Editor Settings
(leaf *user-settings
  :config
  (setq user-full-name "Takuma Katanosaka"
        user-mail-address "bumpbumpbump.v@gmail.com"))


(leaf *language-settings
  :config
  (set-language-environment 'Japanese)
  (prefer-coding-system 'utf-8))

(leaf *editor-settings
  :config
  ;; Confirm to quit this emacs
  (setq confirm-kill-emacs 'y-or-n-p)
  
  ;; Enable backup
  (setq backup-inhibited t)
  
  ;; Delete auto save files
  (setq delete-auto-save-files t)
  
  ;; Exec of omitted command
  (defvar partial-completion-mode "")
  (setq partial-completion-mode 1)
  
  ;; Case-insensitive
  (setq completion-ignore-case t)
  
  ;; Tab width setting
  (setq-default tab-width 2 indent-tabs-mode nil)
  
  ;;Highlight cursor line
  (global-hl-line-mode t)
  
  ;; Set the number of lines to scroll
  (setq scroll-conservatively 1)
  
  (leaf nlinum
    :doc "line number setings"
    :ensure t
    :hook
    (prog-mode-hook . nlinum-mode)
    )
  )

(leaf *deliminator-settings
  :config
  (leaf rainbow-delimiters
    :doc "highlight matching paren with rainbow color"
    :tag "builtin"
    :ensure t
    :hook
    (prog-mode-hook . rainbow-delimiters-mode)
    )
  (leaf paren
    :doc "highlight matching paren"
    :tag "builtin"
    :custom '((show-paren-delay . 0.1)
              (show-paren-style . 'mixed))
    :custom-face
    (show-paren-match '((nil (:background 'unspecified :underline "turquoise"))))
    :global-minor-mode show-paren-mode
    )
  (leaf smartparens
    :doc "括弧の対応の補完"
    :ensure t
    :hook (after-init-hook . smartparens-global-mode)
    :require smartparens-config
    :custom ((electric-pair-mode . nil))
    )
  (leaf highlight-indent-guides
    :doc "インデントのハイライト"
    :ensure t
    :blackout t
    :hook (((prog-mode-hook yaml-mode-hook) . highlight-indent-guides-mode))
    :custom (
             (highlight-indent-guides-method . 'character)
             (highlight-indent-guides-auto-enabled . nil)
             (highlight-indent-guides-responsive . t)
             (highlight-indent-guides-character . ?|)
             )
    )
  )


(leaf company
  :doc "オートコンプリート"
  :req "emacs-24.3"
  :tag "matching" "convenience" "abbrev" "emacs>=24.3"
  :url "http://company-mode.github.io/"
  :emacs>= 24.3
  :ensure t
  :blackout t
  :leaf-defer nil
  :custom (
           (company-idle-delay . 0)
           (company-minimum-prefix-length . 2)
           (company-selection-wrap-around . t)
           )
  :global-minor-mode global-company-mode
  )

(leaf ivy
  :doc "Incremental Vertical completYon"
  :req "emacs-24.5"
  :tag "matching" "emacs>=24.5"
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
    :req "emacs-24.5" "ivy-0.13.0"
    :tag "matching" "emacs>=24.5"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :ensure t
    :bind (("C-s" . swiper)))
  
  (leaf counsel
    :doc "Various completion functions using Ivy"
    :req "emacs-24.5" "swiper-0.13.0"
    :tag "tools" "matching" "convenience" "emacs>=24.5"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :ensure t
    :blackout t
    :bind (("C-S-s" . counsel-imenu)
           ("C-x C-r" . counsel-recentf))
    :custom `((counsel-yank-pop-separator . "\n----------\n")
              (counsel-find-file-ignore-regexp . ,(rx-to-string '(or "./" "../") 'no-group)))
    :global-minor-mode t))


(leaf prescient
  :doc "Better sorting and filtering"
  :req "emacs-25.1"
  :tag "extensions" "emacs>=25.1"
  :url "https://github.com/raxod502/prescient.el"
  :emacs>= 25.1
  :ensure t
  :custom ((prescient-aggressive-file-save . t))
  :global-minor-mode prescient-persist-mode)

(leaf ivy-prescient
  :doc "prescient.el + Ivy"
  :req "emacs-25.1" "prescient-4.0" "ivy-0.11.0"
  :tag "extensions" "emacs>=25.1"
  :url "https://github.com/raxod502/prescient.el"
  :emacs>= 25.1
  :ensure t
  :after prescient ivy
  :custom ((ivy-prescient-retain-classic-highlighting . t))
  :global-minor-mode t)


(leaf magit
  :doc "Gitフロントエンド"
  :bind (("C-x g" . magit-status))
  :ensure t
  :init
  (leaf transient
    :custom
    `(
      ;; (transient-history-file
      ;;  . ,(expand-file-name "transient-history.el" my:d:tmp))
      ;; (transient-levels-file
      ;;  . ,(expand-file-name "transient-levels.el" my:d:tmp))
      ;; (transient-values-file
      ;;  . ,(expand-file-name "transient-values.el" my:d:tmp))
      (transient-force-fixed-pitch . t)
      )
    )
  :custom
  `((magit-completing-read-function . 'magit-builtin-completing-read)
    (magit-refs-show-commit-count   . 'all)
    (magit-log-buffer-file-locked   . t)
    (magit-revision-show-gravatars  . nil)
    )
  )


(leaf flyspell
  :doc "スペルチェック"
  :if (executable-find "aspell")
  :blackout (flyspell-mode . "F")
  :custom
  `((ispell-program-name   . "aspell")
    (ispell-check-comments . nil)
    (ispell-skip-html      . t)
    (ispell-silently-savep . t)
    )
  :bind (:flyspell-mode-map
         ("C-." . nil)
         ("C-," . nil))
  :defer-config
  (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))
  )


(leaf modus-themes
  :ensure t
  :blackout t
  :commands modus-themes-load-theme
  :init
  (modus-themes-load-theme 'modus-vivendi)
  )

(leaf popwin
  :ensure t
  :blackout t)
(leaf direx
  :ensure t
  :blackout t
  :custom
  '(
    (push '(direx:direx-mode :position left :width 35 :dedicated t)
          popwin:special-display-config)
    )
  :preface
  (defun direx:jump-to-project-directory ()
    "If in project, launch direx-project otherwise start direx."
    (interactive)
    (let ((result (ignore-errors
                    (direx-project:jump-to-project-root-other-window)
                    t)))
      (unless result
        (direx:jump-to-directory-other-window))))
  :bind ("<f12>" . direx:jump-to-project-directory)
  )


(leaf *js-settings
  :config
  (leaf web-mode
    :ensure t
    )
  (leaf eglot
    :ensure t
    :config
    (add-hook 'web-mode-hook 'eglot-ensure))
  )


(provide 'init)
