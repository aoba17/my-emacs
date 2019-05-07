;; Global
;; delete-key
(keyboard-translate ?\C-h ?\C-?)
;; meta-key
(when (and (eq system-type 'darwin) (eq window-system 'ns))
  (setq ns-command-modifier (quote meta))
  (setq ns-option-modifier (quote super))
  (set-frame-parameter nil 'alpha 90))
;; show line number
(global-linum-mode t)
;; show line/colomn number on modeline
(column-number-mode)
;; sound
(setq ring-bell-function 'ignore)
;; don't save backup files
(setq make-backup-files nil)
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
 '(package-selected-packages
   (quote
    (magit org-plus-contrib org solarized-theme highlight-symbol nyan-mode beacon rainbow-delimiters helm dired-toggle package-utils migemo zenburn-theme markdown-mode ac-slime auto-complete slime))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

                                        ; set theme
                                        ;(load-theme 'zenburn t)
(load-theme 'solarized-dark)
;; enable auto-complete
(ac-config-default)
;; i-search japanese
(when (require 'migemo nil t)
  (setq migemo-command "/usr/local/bin/cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (load-library "migemo")
  (migemo-init))
;; helm
(define-key global-map
  "\C-xB" 'helm-mini)
;; rainbow-delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
;; beacon
(beacon-mode 1)
(setf beacon-color "#80ffd0")

;; nyan
(nyan-mode 1)
(nyan-start-animation)
;;highlight-symbol
                                        ; 1秒後に自動ハイライト
(setf highlight-symbol-idle-delay 0.5)
                                        ; プログラミングモードでシンボルハイライト
(add-hook 'prog-mode-hook 'highlight-symbol-mode)
                                        ; M-n/M-pでシンボル間移動
(add-hook 'prog-mode-hook 'highlight-symbol-nav-mode)
                                        ; シンボルの置換
(global-set-key (kbd "M-s M-r") 'highlight-symbol-query-replace)

;; dired-mode
(require 'dired)
(define-key dired-mode-map (kbd "(") 'dired-hide-details-mode)
(define-key dired-mode-map (kbd ")") 'dired-hide-details-mode)
(global-set-key "\C-t" 'dired-toggle)
                                        ;(define-key dired-mode-map "e" 'wdired-change-to-wdired-mode)

;;org-mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)

;;magit
(global-set-key (kbd "C-x g") 'magit-status)








;; Common Lisp
;; Read quicklisp
(load (expand-file-name "~/quicklisp/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq slime-lisp-implementation
      '((sbcl ("/usr/local/bin/sbcl") :coding-system utf-8-unix)))

;; load contrib
(add-to-list 'slime-contribs 'slime-fancy)
(add-to-list 'slime-contribs 'slime-cl-indent)

;; enable ac-slime in slime-mode
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))








;; Scheme
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
