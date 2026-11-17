;;; custom.el --- Customize が書き込む設定の置き場  -*- lexical-binding: t; -*-

;;; Commentary:

;; M-x customize 系のコマンドが自動生成する設定だけを置きます。
;; 手書きの設定は init.el に書いてください。

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(apheleia avy blackout breadcrumb company counsel diff-hl
              difftastic dirvish expreg flymake-collection gptel
              highlight-indent-guides hydra ivy ivy-prescient leaf
              leaf-convert leaf-keywords leaf-tree magit markdown-mode
              modus-themes popwin prescient rainbow-delimiters rg
              smartparens sql-indent sqlformat string-inflection swiper
              symbol-overlay tempel terraform-mode treesit-auto undo-fu
              undo-fu-session vundo web-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(show-paren-match ((nil (:background unspecified :underline "turquoise"))) nil "Customized with leaf in `paren' block"))

;;; custom.el ends here
