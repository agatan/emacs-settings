;; Customizations relating to editing a buffer.

;; Lisp-frienfly hippie expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; Highlights matching parenthsis
(show-paren-mode 1)

;; Highlights current line
;; (global-hl-line-mode 1)

;; Interactive search key bindings.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

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

;; yay rainbows!
(rainbow-delimiters-mode t)

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
(require 'auto-complete-config)
(ac-config-default)
(ac-set-trigger-key "TAB")
(add-to-list 'ac-sources 'ac-source-symbols)
(add-to-list 'ac-sources 'ac-source-filename)

