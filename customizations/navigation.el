;; These customizations make it easier for you to navigate files, buffers
;; and options.

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
  (helm-mode 1))


;; ace-isearch
(use-package ace-isearch
  :config
  (global-ace-isearch-mode 1))

(use-package helm-swoop
  :config
  (progn
    (setq helm-swoop-split-with-multiple-windows t)))
