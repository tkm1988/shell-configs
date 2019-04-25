;;=============================
;;        General
;;=============================
;; Make         : 2018/07/22
;; Last Modifiy : 2018/07/22
;; Note         : Deleted repository settings for package.el
;; Ref          : none

(set-language-environment 'Japanese)
(set-keyboard-coding-system 'utf-8)

(setq confirm-kill-emacs 'y-or-n-p)

(setq backup-inhibited t)
(setq delete-auto-save-files t)

(setq partial-completion-mode 1)
(setq completion-ignore-case t)
(setq-default tab-width 2 indent-tabs-mode nil)

;;============================
;;       straight.el
;;============================
;; Make         : 2018/07/22
;; Last Modifiy : 2018/07/22
;; Note         : Attempt to use straight.el
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
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 2)
(setq company-selection-wrap-around t)

;;============================
;;          elpy
;;============================
;; Make         : 2019/04/19
;; Last Modifiy : 2018/04/19
;; Note         : elpy is "Emacs Python Development Environment"

(use-package elpy)
(elpy-enable)

(use-package dockerfile-mode)
(use-package typescript-mode)
