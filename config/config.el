(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(scroll-bar-mode 0)
(tool-bar-mode 0)

(show-paren-mode t)
(setf show-paren-style 'mixed)

(global-hl-line-mode t)

(setq-default cursor-type 'bar)

(set-frame-parameter nil 'fullscreen 'maximized)

(keyboard-translate ?\C-h ?\C-?)

(setq ring-bell-function 'ignore)

(setq make-backup-files nil)
(setq auto-save-default nil)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(setq-default indent-tabs-mode nil)

(ffap-bindings)

(when (and (eq system-type 'darwin) (eq window-system 'ns))
  (setq ns-command-modifier (quote meta))
  (setq ns-option-modifier (quote super))
  (set-frame-parameter nil 'alpha 90)
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

(use-package neotree
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

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(use-package all-the-icons)

(use-package doom-themes
  :custom
  (doom-themes-enable-italic t)
  (doom-themes-enable-bold t)
  :config
  (load-theme 'doom-dracula t)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

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
    (add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)
    (smartparens-strict-mode nil)))

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

(use-package ace-window
    :ensure t
    :defer 1
    :config
    (set-face-attribute
     'aw-leading-char-face nil
     :foreground "deep sky blue"
     :weight 'bold
     :height 3.0)
    (set-face-attribute
     'aw-mode-line-face nil
     :inherit 'mode-line-buffer-id
     :foreground "lawn green")
    (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l)
          aw-dispatch-always t
          aw-dispatch-alist
          '((?x aw-delete-window "Ace - Delete Window")
            (?c aw-swap-window "Ace - Swap Window")
            (?n aw-flip-window)
            (?v aw-split-window-vert "Ace - Split Vert Window")
            (?h aw-split-window-horz "Ace - Split Horz Window")
            (?m delete-other-windows "Ace - Maximize Window")
            (?g delete-other-windows)
            (?b balance-windows)
            (?u (lambda ()
                  (progn
                    (winner-undo)
                    (setq this-command 'winner-undo))))
            (?r winner-redo)))

    (when (package-installed-p 'hydra)
      (defhydra hydra-window-size (:color red)
        "Windows size"
        ("h" shrink-window-horizontally "shrink horizontal")
        ("j" shrink-window "shrink vertical")
        ("k" enlarge-window "enlarge vertical")
        ("l" enlarge-window-horizontally "enlarge horizontal"))
      (defhydra hydra-window-frame (:color red)
        "Frame"
        ("f" make-frame "new frame")
        ("x" delete-frame "delete frame"))
      (defhydra hydra-window-scroll (:color red)
        "Scroll other window"
        ("n" joe-scroll-other-window "scroll")
        ("p" joe-scroll-other-window-down "scroll down"))
      (add-to-list 'aw-dispatch-alist '(?w hydra-window-size/body) t)
      (add-to-list 'aw-dispatch-alist '(?o hydra-window-scroll/body) t)
      (add-to-list 'aw-dispatch-alist '(?\; hydra-window-frame/body) t))
    (ace-window-display-mode t))

;; Load quicklisp
(load (expand-file-name "~/quicklisp/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq slime-lisp-implementation
      '((sbcl ("/usr/local/bin/sbcl") :coding-system utf-8-unix)))
;; load contrib
(slime-setup '(slime-fancy slime-company slime-cl-indent))

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

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t))
