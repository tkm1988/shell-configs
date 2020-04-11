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
(global-hl-line-mode t)

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

(let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 3))
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
;;        Apex-mode
;;============================
;; Make         : 2020/04/11
;; Last Modifiy : 2020/04/11
;; Note         : Setting up for programming language "Apex"
;; (straight-use-package
;;  '(apex-mode :type git :host github :repo "ctomo/apex-mode"))
;; (use-package apex-mode)

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

;; Enable the plug-in "flycheck"
(use-package flycheck)

;; Automatically check syntax
(setq flycheck-check-syntax-automatically
      '(save idle-change mode-enabled))

;; Check after 3 minutes
(setq flycheck-idle-change-delay 3)

;; Add a linter for JS
(flycheck-add-mode 'javascript-eslint 'web-mode)

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

;; Apply web-mode to ejs, js, jsx, ts, html, php and vue
(add-to-list 'auto-mode-alist '("\\.(?:html|jsx?|ejs|php|ts|vue)$" . web-mode))

;; Change .jsx edit mode if file type is .js
(defvar web-mode-content-types-alist
  '(("jsx" . "\\.js[x]?\\'")))

;; Setting about comment-out
(add-hook 'web-mode-hook
  '(lambda ()
    (add-to-list 'web-mode-comment-formats '("jsx" . "//" ))))

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
;; Last Modifiy : 2020/04/11
;; Note         : Asynchronous automatically fix lint errors to use eslint
;; Ref          : https://www.m3tech.blog/entry/emacs-web-service

;; Exec : M-x eslint-fix-file
(defun eslint-fix-file ()
  (interactive)
  (call-process-shell-command
   (mapconcat 'shell-quote-argument
              (list "eslint" "--fix" (buffer-file-name)) " ") nil 0))

;; Revert buffer after executing eslint-fix-file
(defun eslint-fix-file-and-revert ()
  (interactive)
  (eslint-fix-file)
  (revert-buffer t t))

;;============================
;;     dockerfile-mode
;;============================
;; Make         : 2020/04/11
;; Last Modifiy : 2020/04/11
;; Note         : Use dockerfile-mode

(use-package dockerfile-mode)
