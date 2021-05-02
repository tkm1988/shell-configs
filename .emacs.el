;;=============================
;;        General
;;=============================
;; Make         : 2018/07/22
;; Last Modifiy : 2020/04/11
;; Note         : General settings for emacs
;; Ref          : none

;; Language Settings
(set-language-environment 'Japanese)
(set-keyboard-coding-system 'utf-8)

;; Confirm to quit this emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; Enable backup
(setq backup-inhibited t)

;; Delete auto save files
(setq delete-auto-save-files t)

;; Exec of omitted command
(setq partial-completion-mode 1)

;; Case-insensitive
(setq completion-ignore-case t)

;; Tab width setting
(setq-default tab-width 2 indent-tabs-mode nil)

;; Highlight cursor line
;;(global-hl-line-mode t)

;; Highlight inside brackets
(show-paren-mode 1)

(setq show-paren-style 'mixed)
(set-face-attribute 'show-paren-match nil
                    :background 'unspecified
                    :underline "turquoise")

;; Set the number of lines to scroll
(setq scroll-conservatively 1)

;;============================
;;       straight.el
;;============================
;; Make         : 2018/07/22
;; Last Modifiy : 2018/07/22
;; Note         : Attempt to use straight.el when you install packages
;; Ref          : https://nukosuke.hatenablog.jp/entry/straight-el

(setq straight-recipes-gnu-elpa-use-mirror t
      straight-check-for-modifications     '(find-when-checking))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(use-package git) ;; ensure we can install from git sources

(global-linum-mode t)

;;============================
;;         company
;;============================
;; Make         : 2019/04/19
;; Last Modifiy : 2018/04/19
;; Note         : Attempt to use auto-complite by company.

(straight-use-package 'company)
(use-package company)
(global-company-mode)
(setq company-idle-delay 1)
(setq company-minimum-prefix-length 2)
(setq company-selection-wrap-around t)


;;============================
;; ivy.el & counsel.el & swiper.el
;;============================
;; Make         : 2021/04/30
;; Last Modifiy : 2021/04/30
;; Note         : -

(straight-use-package 'ivy)
(use-package ivy)
(ivy-mode 1)


;;============================
;;     dumb-jump-mode
;;============================
;; Make         : 2021/04/30
;; Last Modifiy : 2021/04/30
;; Note         : -

(straight-use-package 'dumb-jump)
(use-package dumb-jump)
(setq dumb-jump-mode t)


;;============================
;;         magit
;;============================
;; Make         : 2020/04/12
;; Last Modifiy : 2020/04/12
;; Note         : git plug-in
;; Ref          : https://magit.vc/manual/

;; DEBUG : First, it must install magit/transient.
(straight-use-package
 '(transient :type git :host github :repo "magit/transient"))

;; Install from github to use straight.el
(straight-use-package
 '(magit :type git :host github :repo "magit/magit"))
(use-package magit)


;;============================
;;          lsp-mode
;;============================
;; Make         : 2021/04/30
;; Last Modifiy : 2021/04/30
;; Note         : -

(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ivy)
(use-package lsp-mode)
(use-package lsp-ivy)


;;============================
;;         web-mode
;;============================
;; Make         : 2020/04/11
;; Last Modifiy : 2020/04/11
;; Note         :
;; Ref          : https://www.m3tech.blog/entry/emacs-web-service

;; Install from github
(straight-use-package
 '(web-mode :type git :host github :repo "fxbois/web-mode"))

;; Enable web-mode
(use-package web-mode)

;; Apply web-mode to files I often use.
(add-to-list 'auto-mode-alist '("\\.js[x]?$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.htm[l]?$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[s]?css$" . web-mode))

;; Setting about comment-out
(add-hook 'web-mode-hook
          '(lambda ()
             (add-to-list 'web-mode-comment-formats '("js" . "//" ))))

;; Set language-server-protocol
(add-hook 'web-mode-hock #'lsp)


;;============================
;;   prettier-js for Emacs
;;============================
;; Make         : 2021/04/30
;; Last Modifiy : 2021/04/30
;; Note         : -

(straight-use-package 'prettier-js)


;;============================
;;         flycheck
;;============================
;; Make         : 2020/04/11
;; Last Modifiy : 2020/04/11
;; Note         : Syntax chacking tool for Emacs
;; Ref          : https://www.m3tech.blog/entry/emacs-web-service

;; Install from github to use straight.el
(straight-use-package
 '(flycheck :type git :host github :repo "flycheck/flycheck"))

;; Automatically check syntax
(setq flycheck-check-syntax-automatically
      '(save idle-change mode-enabled))

;; Check after 1 seconds.
(setq flycheck-idle-change-delay 1)

;; Enable the plug-in "flycheck"
(global-flycheck-mode)

;; Look at the node-module of the project you are working on and load the respective settings appropriately.
(straight-use-package 'add-node-modules-path)
(add-hook 'web-mode-hook #'add-node-modules-path)
     
;; Load configuration file for stylelint
(straight-use-package 'projectile)
(projectile-mode +1)
(defun setup-stylelint-config ()
  "If project root path was found, it's set up stylelint config file's path to flycheck config var."
  (let ((root (ignore-errors (projectile-project-root))))
    (when root
      (custom-set-variables '(flycheck-stylelintrc (concat root "stylelint.config.js"))))))

(add-hook 'web-mode-hook 'setup-stylelint-config)

;; Add linter and formatter
(eval-after-load 'web-mode
  '(progn
     ;; Add a linter for CSS/SCSS
     (flycheck-add-mode 'css-stylelint 'web-mode)
     (flycheck-add-mode 'scss-stylelint 'web-mode)

     ;; Add a linter for JS
     (flycheck-add-mode 'javascript-eslint 'web-mode)

     ;; Use HTML Tidy
     ;; (flycheck-add-mode 'html-tidy 'web-mode)

     ;; Use prittier
     (add-hook 'web-mode-hook #'prettier-js-mode)))


;;============================
;;         flyspell
;;============================
;; Make         : 2021/04/30
;; Last Modifiy : 2021/04/30
;; Note         : -

;; prog-mode なら常に ON
(add-hook 'prog-mode-hook 'flyspell-mode)

;; ispell の後継である aspell を使う。
;; CamelCase でもいい感じに spellcheck してくれる設定を追加
;; See: https://stackoverflow.com/a/24878128/8888451
(setq-default ispell-program-name "aspell")
(eval-after-load "ispell"
  '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))
(setq ispell-program-name "aspell"
      ispell-extra-args
      '("--sug-mode=ultra" "--lang=en_US" "--run-together" "--run-together-limit=5" "--run-together-min=2"))


;;============================
;;     git-gutter-plus
;;============================
;; Make         : 2021/04/30
;; Last Modifiy : 2021/05/02
;; Note         : -

(straight-use-package 'git-gutter+)

;; Always enable
(global-git-gutter+-mode 1)

;; modify された箇所で実行すると、diff を inline で見ることができる
(global-set-key (kbd "C-x C-v") 'git-gutter+-show-hunk-inline-at-point)


;;============================
;;   color-identifiers-mode
;;============================
;; Make         : 2021/04/30
;; Last Modifiy : 2021/04/30
;; Note         : -

(straight-use-package 'color-identifiers-mode)
(add-hook 'after-init-hook 'global-color-identifiers-mode)


;;============================
;;  rainbow-delimiters-mode
;;============================
;; Make         : 2021/04/30
;; Last Modifiy : 2021/04/30
;; Note         : -

(straight-use-package 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)


;;============================
;;          elpy
;;============================
;; Make         : 2019/04/19
;; Last Modifiy : 2020/04/11
;; Note         : elpy is "Emacs Python Development Environment"

;; Enable elpy
(use-package elpy)
(elpy-enable)

;; Use flycheck
(when (require 'flycheck nil t)
  (remove-hook 'elpy-modules 'elpy-module-flymake)
  (add-hook 'elpy-mode-hook 'flycheck-mode))  

;;============================
;;     eslint fix file
;;============================
;; Make         : 2020/04/11
;; Last Modify  : 2020/04/11
;; Note         : Asynchronous automatically fix lint errors to use eslint
;; Ref          : https://www.m3tech.blog/entry/emacs-web-service

;; Exec : M-x eslint-fix-file
(defun eslint-fix-file ()
  (interactive)
  '(progn
     (call-process-shell-command
      (mapconcat 'shell-quote-argument
                 (list "eslint" "--fix" (buffer-file-name)) " ") nil 0)
     (shell-command (concat "stylelint --fix --quiet " (buffer-file-name)))))

;; Revert buffer after executing eslint-fix-file
(defun eslint-fix-file-and-revert ()
  (interactive)
  (eslint-fix-file)
  (revert-buffer t t))

;;============================
;;     dockerfile-mode
;;============================
;; Make         : 2020/04/11
;; Last Modify  : 2020/04/11
;; Note         : Use dockerfile-mode

(use-package dockerfile-mode)
