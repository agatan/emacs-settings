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

;; shell-pop settings
(custom-set-variables
 '(shell-pop-default-directory "~")
 '(shell-pop-shell-type '("ansi-term" "*ansi-term*" (lambda () (ansi-term shell-pop-term-shell))))
'(shell-pop-term-shell (substring (shell-command-to-string "which zsh 2>/dev/null") 0 -1)))
(global-set-key (kbd "C-c c") 'shell-pop)
