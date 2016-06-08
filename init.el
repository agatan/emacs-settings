;; Determine 'user-emacs-directory'
(when load-file-name
  (setq user-emacs-directory (expand-file-name
                              (file-name-directory load-file-name))))


(eval-when-compile (require 'cl))

(when load-file-name
  (setq user-emacs-directory (expand-file-name
                              (file-name-directory load-file-name))))

;; Turn off mouse interfaces
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;;;;
;; Packages
;;;;

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/"))

;; Load and activate emacs packages.
(package-initialize)

;; Download the Elpa archive description if needed.
(when (not package-archive-contents)
  (package-refresh-contents))

(unless (require 'use-package nil t)
  (defmacro use-package (&rest args)
    (error "Please install use-package")))
(setq use-package-always-ensure t)
(setq use-package-verbose t)


;; Place manually downloaded elisp files in ~/.emacs.d/vendor.
;;
;; For example, if you donwload yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to use it:
;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'load-path "~/.emacs.d/vendor")


;;;;
;; Customization
;;;;


;; Set up exec-shell-from-shell
(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :config
    (progn
      (exec-path-from-shell-initialize)
      (exec-path-from-shell-copy-envs '("PATH")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This setting make buffer names to unique.
;; Adds directory name when same file names exist.
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Turn on recent file mode.
(use-package recentf
  :config
  (progn
    (setq recentf-save-file (concat user-emacs-directory ".recentf"))
    (require 'recentf)
    (recentf-mode 1)
    (setq recentf-max-menu-items 40)))

;; projectile everywhere
(use-package projectile
  :config
  (projectile-global-mode))


;; helm settings
(use-package helm
  :config
  (progn
    (helm-mode 1)
    (define-key global-map (kbd "M-x") 'helm-M-x)
    (define-key global-map (kbd "C-x C-f") 'helm-find-files)
    (define-key global-map (kbd "C-c C-f") 'helm-mini)
    (define-key global-map (kbd "C-x C-r") 'helm-recentf)
    (define-key global-map (kbd "C-x b") 'helm-buffers-list)
    (define-key helm-map (kbd "C-h") 'delete-backward-char)
    (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
    (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
    (define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
    (define-key helm-find-files-map (kbd "C-z") 'helm-select-action)))

;; ace-isearch
(use-package ace-isearch
  :config
  (global-ace-isearch-mode 1))

(use-package helm-swoop
  :config
  (progn
    (setq helm-swoop-split-with-multiple-windows t)))

;; mark position and jump to them
(use-package helm-bm
  :commands (helm-bm))

(defun bm-toggle-or-helm ()
  (interactive)
  (bm-toggle)
  (when (eq last-command 'bm-toggle-or-helm)
    (helm-bm)))

(use-package bm
  :commands (bm-toggle bm-next bm-previous)
  :bind (("M--" . bm-toggle-or-helm)
         ("M-[" . bm-previous)
         ("M-]" . bm-next)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn off the menu bar at the top of each frame.
(menu-bar-mode -1)

;; Turn off the graphical tool bar at the top of each frame.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Color Themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
(load-theme 'wombat)

(use-package helm-themes
  :commands helm-themes)

;; increse font size for better readability
(cond
 ((or (eq system-type 'mac) (eq system-type 'darwin))
  (set-face-attribute 'default nil :family "Ricty" :height 150))
 ((eq system-type 'gnu/linux) (set-face-attribute 'default nil :family "Ricty Diminished" :height 100)))


;; These settings relate to how emacs interacts with your operation system
(setq
 ;; makes killing/yanking interact with the clipboard
 x-select-enable-clipboard t

 ;; I'm not sure what it is.
 x-select-enable-primary t
 save-interprogram-paste-before-kill t
 apropos-do-all t
 mouse-yank-at-point t)

;; No cursor blinking.
(blink-cursor-mode 0)

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; don't pop up font menu
(global-set-key (kbd "s-t") '(lambda () (interactive)))

;; no bel
(setq ring-bell-function 'ignore)

;; Turn off scroll bar
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; show column
(column-number-mode 1)

;; Show trailing white spaces
(setq-default show-trailing-whitespace t)

;; popup windows
(use-package popwin
  :config
  (progn
    (popwin-mode 1)
    (setq display-buffer-function 'popwin:display-buffer)
    (setq popwin:popup-window-position 'bottom)))

;; filer
(use-package direx
  :bind (("C-c C-j" . direx:jump-to-directory-other-window))
  :config
  (push '(direx:direx-mode :position left :width 30 :dedicated t)
        popwin:special-display-config))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizations relating to editing a buffer.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Lisp-frienfly hippie expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; Highlights matching parenthsis
(show-paren-mode 1)

;; Don't use hard tabs
(setq-default indent-tabs-mode nil)

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
(require 'saveplace)
(setq-default save-place t)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

;; Emacs can automatically create backup files.
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)

;; comments toggle
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;; use 2 spaces for tabs
(defun die-tabs (n)
  (interactive "nSpaces count:")
  (set-variable 'tab-width n)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

(setq electric-indent-mode nil)


;; Ctrl-H to backspace
(global-set-key (kbd "C-h") 'backward-delete-char)

;; tab width
(custom-set-variables '(tab-width 4))

;; C-k delete a line and '\n' if cursor is on the head of line
(setq kill-whole-line t)

;; Auto-complete settings
(use-package company
  :bind (("<tab>" . company-indent-or-complete-common))
  :config
  (global-company-mode 1)
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay 0.3)
  (define-key company-active-map (kbd "C-h") nil)
  (set-face-attribute 'company-tooltip nil
                      :foreground "black" :background "lightgrey")
  (set-face-attribute 'company-tooltip-common nil
                      :foreground "black" :background "lightgrey")
  (set-face-attribute 'company-tooltip-common-selection nil
                      :foreground "white" :background "steelblue")
  (set-face-attribute 'company-tooltip-selection nil
                      :foreground "black" :background "steelblue")
  (set-face-attribute 'company-preview-common nil
                      :background nil :foreground "lightgrey" :underline t)
  (set-face-attribute 'company-scrollbar-fg nil
                      :background "orange")
  (set-face-attribute 'company-scrollbar-bg nil
                      :background "gray40"))

(use-package company-quickhelp
  :config
  (company-quickhelp-mode 1))

(use-package flycheck)

;; snippet
(use-package yasnippet
  :config
  (define-key yas-keymap (kbd "<tab>") nil)
  (yas-global-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MISC (hard to categorize other categories.)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

;; On OS X, meta key is option.
;; Change meta key binding to left command.
(when (eq system-type 'darwin)
  (setq default-input-method "MacOSX")
  (setq ns-command-modifier 'meta)
  (setq ns-alternate-modifier 'super))
(when (eq system-type 'gnu/linux)
  (setq x-super-keysym 'meta)
  (setq x-meta-keysym 'super))

;; shell-pop settings
(use-package shell-pop
  :config
  (progn
    (custom-set-variables
     '(shell-pop-default-directory "~")
     '(shell-pop-shell-type '("ansi-term" "*ansi-term*" (lambda () (ansi-term shell-pop-term-shell))))
     '(shell-pop-term-shell (substring (shell-command-to-string "which zsh 2>/dev/null") 0 -1))))
  :bind ("C-c c" . shell-pop))

(use-package magit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SKK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ddskk
  :config
  (progn
    (require 'skk-vars)
    (autoload 'skk-mode "skk" nil t)
    (autoload 'skk-auto-fill-mode "skk" nil t)
    (autoload 'skk-isearch-mode-setup "skk-isearch" nil t)
    (autoload 'skk-isearch-mode-cleanup "skk-isearch" nil t)

    ;; Enterキーを押したときには確定する
    (setq skk-egg-like-newline t)
    ;; 辞書登録のとき， 余計な送り仮名を送らないようにする
    (setq skk-check-okurigana-on-touroku 'auto)
    ;; look コマンド を使った検索をする
    (setq skk-use-look t)
    ;; 半角数字
    (setq skk-number-style nil)
    ;; 句読点に. , を使う
    (setq skk-kutouten-type 'en))
  :bind
  ("C-x C-j" . skk-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :bind (("C-c a t" . org-agenda))
  :config
  (progn
    (setq org-directory "~/Dropbox/org/")
    (setq org-agenda-files (list org-directory))
    (setq org-src-fontify-natively t)))
(use-package open-junk-file
  :bind (("C-x j" . open-junk-file))
  :config
  (progn
    (setq open-junk-file-format "~/Dropbox/org/%Y-%m%d-memo.")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lnaguages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;
;; Lisp
;;;;;;;;;;;;;;;;;;;;
(use-package paredit
  :defer t
  :config
  (define-key paredit-mode-map (kbd "C-h") 'paredit-backward-delete)

  (defun conditionally-enable-paredit-mode ()
    (if (eq this-command 'eval-expression)
        (paredit-mode 1)))
  (add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode))

(use-package eldoc
  :defer t
  :config
  (setq eldoc-delay 0.1
        eldoc-minor-mode-string ""))

(use-package rainbow-delimiters)

(defun rainbow-delimiters-using-stronger-colors ()
  (interactive)
  (require 'cl-lib)
  (require 'color)
  (cl-loop
   for index from 1 to rainbow-delimiters-max-face-count
   do
   (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
     (cl-callf color-saturate-name (face-foreground face) 40))))

(defun my/rainbow-delimiters-mode-with-stronger-colors ()
  (rainbow-delimiters-mode 1)
  (rainbow-delimiters-using-stronger-colors))

(defun my/lisp-mode-defaults ()
  (paredit-mode 1)
  (my/rainbow-delimiters-mode-with-stronger-colors)
  (eldoc-mode 1))
(defun my/lisp-mode-hook ()
  (my/lisp-mode-defaults))

(add-hook 'emacs-lisp-mode-hook 'my/lisp-mode-hook)

;;;;;;;;;;;;;;;;;;;;
;; Clojure
;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.cljs$'" . clojure-mode))

(use-package clojure-mode
  :init
  (add-hook 'clojure-mode-hook 'my/lisp-mode-hook))

(use-package cider
  :init
  (add-hook 'cider-mode-hook #'clj-refactor-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook #'my/lisp-mode-hook)
  :diminish subword-mode
  :config
  (setq nrepl-log-messages t
        cider-repl-display-in-current-window t
        cider-repl-use-clojure-font-lock t
        cider-prompt-save-file-on-load 'always-save
        cider-font-lock-dynamically '(macro core function var)
        cider-overlays-use-font-lock t)
  (cider-repl-toggle-pretty-printing))

(use-package cider-eval-sexp-fu)

(use-package clj-refactor
  :diminish clj-refactor-mode
  :config (cljr-add-keybindings-with-prefix "C-c j"))

(defun my/clojure-mode-hook ()
  (cider-mode 1)
  (rainbow-delimiters-mode 1)
  (smart-newline-mode 1))
(add-hook 'clojure-mode-hook 'my/clojure-mode-hook)

(when (boundp 'popwin:special-display-config)
  (push '("*cider-apropos*" :noselect t) popwin:special-display-config)
  (push '("*cider-macroexpansion*" :noselect t) popwin:special-display-config)
  (push '("*cider-error*" :noselect t :height 20) popwin:special-display-config)
  (push '("*cider-doc*" :noselect t :height 20) popwin:special-display-config)
  (push '(cider-macroexpansion-mode :noselect t) popwin:special-display-config))

;;;;;;;;;;;;;;;;;;;;
;; common lisp
;;;;;;;;;;;;;;;;;;;;
(use-package slime
  :commands (slime)
  :mode (("\\.lisp$'" . slime-mode)
         ("\\.ros$'" . slime-mode))
  :config
  (progn
    (setq inferior-lisp-program "ros run")
    (setq slime-contribs '(slime-fancy))

    ;; popwin settings
    (when (boundp 'popwin:special-display-config)
      (dolist (buf '(("*slime-apropos*")
                     ("*slime-macroexpansion*")
                     ("*slime-description*")
                     ("*slime-compilation*")
                     ("*slime-xref*")
                     (sldb-mode :stick t)
                     (slime-connection-list-mode)))
        (push buf popwin:special-display-config)))))

(use-package slime-company
  :init
  (add-hook 'slime-mode-hook #'slime-company))


;;;;;;;;;;;;;;;;;;;;
;; c/c++
;;;;;;;;;;;;;;;;;;;;
(add-hook 'c-mode-common-hook
          (lambda ()
            (setq c-default-style "k&r")
            (setq indent-tabs-mode nil)
            (setq c-basic-offset 2)))

(add-to-list 'auto-mode-alist '("\\.hpp$'" . c++-mode))

(use-package irony
  :config
  (custom-set-variables '(irony-additional-clang-options '("-std=c++14")))
  (add-to-list 'company-backends 'company-irony)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (add-hook 'c-mode-common-hook 'irony-mode))

;;;;;;;;;;;;;;;;;;;;
;; rust
;;;;;;;;;;;;;;;;;;;;
(use-package rust-mode
  :mode (("\\.rs$'" . rust-mode))
  :config
  (progn
    (use-package flycheck-rust)
    (use-package racer)
    (setq racer-cmd "~/rust/racer/target/release/racer")
    (setq racer-rust-src-path "~/rust/rust/src/")
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (add-hook 'racer-mode-hook #'company-mode)))

;;;;;;;;;;;;;;;;;;;;
;; Haskell
;;;;;;;;;;;;;;;;;;;;
(use-package haskell-mode
  :mode
  (("\\.hs\\'" . haskell-mode)
   ("\\.cabal\\'" . haskell-cabal-mode))
  :config
  (progn
    (use-package ghc)
    (add-hook 'haskell-mode-hook (lambda () (ghc-init)))
    (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
    (add-hook 'haskell-mode-hook 'haskell-doc-mode)
    (add-hook 'haskell-mode-hook 'font-lock-mode)
    (add-hook 'haskell-mode-hook 'imenu-add-menubar-index)))

;;;;;;;;;;;;;;;;;;;;
;; OCaml
;;;;;;;;;;;;;;;;;;;;
(use-package tuareg
  :mode (("\\.ml" . tuareg-mode))
  :init
  (progn
    (add-hook 'tuareg-mode-hook
              '(lambda ()
                 (local-set-key "\C-c;" 'ocamlspot-query)
                 (local-set-key "\C-c:" 'ocamlspot-query-interface)
                 (local-set-key "\C-c'" 'ocamlspot-query-uses)
;                 (local-set-key "\C-c\C-t" 'ocamlspot-type)
                 (local-set-key "\C-c\C-i" 'ocamlspot-xtype)
                 (local-set-key "\C-c\C-y" 'ocamlspot-type-and-copy)
                 (local-set-key "\C-ct" 'caml-types-show-type)
                 (local-set-key "\C-cp" 'ocamlspot-pop-jump-stack)))
    (add-hook 'tuareg-mode-hook 'merlin-mode))
  :config
  (setq opam-share
        (substring
         (shell-command-to-string "opam config var share 2> /dev/null")
         0 -1))
  (add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
  (require 'utop)
  (require 'ocp-indent)
  (require 'ocamlspot)
  (require 'merlin))

;;;;;;;;;;;;;;;;;;;;
;; nim
;;;;;;;;;;;;;;;;;;;;
(use-package nim-mode
  :mode (("\\.nim$'" . nim-mode)))

;;;;;;;;;;;;;;;;;;;;
;; Python
;;;;;;;;;;;;;;;;;;;;
(use-package python-mode)
(use-package company-jedi
  :init
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t)
  (setq jedi:use-shortcuts t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FINISHED!!!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "init.el loaded!!")

(add-hook 'after-init-hook
          (lambda ()
            (message "init time: %.3f sec"
                     (float-time (time-subtract after-init-time before-init-time)))))
