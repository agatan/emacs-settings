;;;; setup-clojure.el

(add-to-list 'auto-mode-alist '("\\.cljs$'" . clojure-mode))

(use-package clojure-mode
  :defer t
  :config
  (progn
    (use-package smart-newline)
    (use-package clojure-mode-extra-font-locking)
    (use-package cider
      :config
      (progn
        (setq nrepl-hide-special-buffers t)
        (setq nrepl-buffer-name-show-port t)))
    (use-package ac-cider)))

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
