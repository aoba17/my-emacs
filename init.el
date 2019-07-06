;; Global
;; delete-key
(keyboard-translate ?\C-h ?\C-?)
;; meta-key
(when (and (eq system-type 'darwin) (eq window-system 'ns))
  (setq ns-command-modifier (quote meta))
  (setq ns-option-modifier (quote super))
  (set-frame-parameter nil 'alpha 90))
;; show line number
;;(global-linum-mode t)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
;; show line/colomn number on modeline
(column-number-mode)
;; sound
(setq ring-bell-function 'ignore)
;; don't save backup files
(setq make-backup-files nil)
(setq auto-save-default nil)
;; font
(when (eq window-system 'ns)
  (let ((font-name "Ricty Diminished")
        (font-size 18))
    (set-frame-font (concat font-name
                            "-"
                            (number-to-string font-size)))
    ;; 全角かな設定
    (set-fontset-font (frame-parameter nil 'font)
                      'japanese-jisx0208
                      (font-spec :family font-name :size font-size))
    ;; 半角ｶﾅ設定
    (set-fontset-font (frame-parameter nil 'font)
                      'katakana-jisx0201
                      (font-spec :family font-name :size font-size))))


;; shell
(setq shell-file-name "/usr/local/bin/zsh")
(define-key global-map
  "\C-cT" 'ansi-term)
;; windmove
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
;; hide appiarance
(scroll-bar-mode 0)
(tool-bar-mode 0)
;; tab → space
(setq-default indent-tabs-mode nil)
;; highright current line
(global-hl-line-mode t)
;; paren highlight
(show-paren-mode t)
(setf show-paren-style 'mixed)
;; cursor
(setq-default cursor-type 'bar) 
;; full screen
(set-frame-parameter nil 'fullscreen 'maximized)
;; enable dir search at C-x C-f
(ffap-bindings)



;; Package Management
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
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
    (eyebrowse company-quickhelp slime-company company smartparens exec-path-from-shell cider projectile doom-themes doom-modeline all-the-icons neotree use-package multiple-cursors dash-at-point web-mode undo-tree magit org-plus-contrib org solarized-theme highlight-symbol nyan-mode beacon rainbow-delimiters helm package-utils migemo markdown-mode ac-slime auto-complete slime))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; set theme
(use-package all-the-icons)
(use-package neotree
  :bind (("C-q" . neotree-toggle))
  :config
  (setq neo-show-hidden-files t)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find git project root."))))
 (global-set-key (kbd "C-q") 'neotree-project-dir)

(use-package doom-themes
  :custom
  (doom-themes-enable-italic t)
  (doom-themes-enable-bold t)
;;  :custom-face
;;  (doom-modeline-bar ((t (:background "#6272a4"))))
  :config
  (load-theme 'doom-dracula t)
  (doom-themes-neotree-config)
  (doom-themes-org-config))
(use-package doom-modeline
      :ensure t
      :hook (after-init . doom-modeline-mode))

;; (use-package auto-complete
;;   :config
;;   (ac-config-default))

(use-package migemo
  :config
  (setq migemo-command "/usr/local/bin/cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (load-library "migemo")
  (migemo-init))

(use-package helm
  :bind (("\C-xB" . helm-mini)))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package beacon
  :config
  (beacon-mode 1)
  (setf beacon-color "#80ffd0"))

(use-package nyan-mode
  :config
  (nyan-mode 1)
  (nyan-start-animation))

(use-package highlight-symbol
  :bind (("M-s M-r" . highlight-symbol-query-replace))
  :init
  ;; プログラミングモードでシンボルハイライト
  (add-hook 'prog-mode-hook 'highlight-symbol-mode)
  ;; M-n/M-pでシンボル間移動
  (add-hook 'prog-mode-hook 'highlight-symbol-nav-mode)  
  :config
  ;; 0.5秒後に自動ハイライト
  (setf highlight-symbol-idle-delay 0.5))

(use-package org-mode
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c b" . org-switchb)))

(use-package magit
  :bind (("C-x g" . magit-status)))

(use-package undo-tree
  :bind (("M-/" . undo-tree-redo))
  :config
  (global-undo-tree-mode t))

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t))

(use-package dash-at-point
  :bind (("\C-cd" . dash-at-point)))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(use-package smartparens-config
  :ensure smartparens
  :config
  (progn
    (show-smartparens-global-mode t)
    (define-key sp-keymap (kbd "M-s-<backspace>") 'sp-unwrap-sexp)
    (define-key sp-keymap (kbd "s-<backspace>") 'sp-backward-unwrap-sexp)
    (define-key sp-keymap (kbd "C-s-p") 'sp-add-to-previous-sexp)
    (define-key sp-keymap (kbd "C-s-n") 'sp-add-to-next-sexp)
    (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
    (add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)))


(use-package company
  :config
  (global-company-mode)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t))

(use-package company-quickhelp
  :config
  (company-quickhelp-mode))

(use-package eyebrowse
  :diminish eyebrowse-mode
  :config
  (progn
    (eyebrowse-mode t)
    (define-key eyebrowse-mode-map (kbd "C-M-.") 'eyebrowse-next-window-config)
    (define-key eyebrowse-mode-map (kbd "C-M-,") 'eyebrowse-prev-window-config)
    (define-key eyebrowse-mode-map (kbd "C-c w d") 'eyebrowse-close-window-config)
    (setq eyebrowse-new-workspace t)))

;;; Common Lisp
;; Load quicklisp
(load (expand-file-name "~/quicklisp/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq slime-lisp-implementation
      '((sbcl ("/usr/local/bin/sbcl") :coding-system utf-8-unix)))
;; load contrib
(slime-setup '(slime-fancy slime-company slime-cl-indent))


;;; Scheme
;; gauche
(setq process-coding-system-alist
      (cons '("gosh" utf-8 . utf-8) process-coding-system-alist))
(setq scheme-program-name "/usr/local/bin/gosh -i")

(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)

(defun scheme-other-window ()
  "Run Gauche on other window"
  (interactive)
  (let ((buf-name (buffer-name (current-buffer))))
    (scheme-mode)
    (switch-to-buffer-other-window
     (get-buffer-create "*scheme*"))
    (run-scheme scheme-program-name)
    (switch-to-buffer-other-window
     (get-buffer-create buf-name))))

(define-key global-map
  "\C-cG" 'scheme-other-window)
