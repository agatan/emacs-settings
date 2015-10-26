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
  (progn
    (helm-mode 1)
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









