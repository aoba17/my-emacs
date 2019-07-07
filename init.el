;; Package Management
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory Comments.
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default)))
 '(doom-themes-enable-bold t)
 '(doom-themes-enable-italic t)
 '(package-selected-packages
   (quote
    (ace-window eyebrowse company-quickhelp slime-company company smartparens exec-path-from-shell cider projectile doom-themes doom-modeline all-the-icons neotree use-package multiple-cursors dash-at-point web-mode undo-tree magit org-plus-contrib org solarized-theme highlight-symbol nyan-mode beacon rainbow-delimiters helm package-utils migemo markdown-mode ac-slime auto-complete slime))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Orgファイルからの設定読み込み
;; config/config.org 参照
(org-babel-load-file (expand-file-name
                      "config.org"
                      (concat user-emacs-directory
                              "config/")))
