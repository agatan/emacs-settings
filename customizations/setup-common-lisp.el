;;;; setup for editting common lisp

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
        (push buf popwin:special-display-config)))

    ;; auto completion for common lisp with slime
    (use-package ac-slime
      :config
      (progn
        (add-hook 'slime-mode-hook 'set-up-slime-ac)
        (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)))))
