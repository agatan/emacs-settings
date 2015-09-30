;; These customizations make it easier for you to navigate files, buffers
;; and options.

;; This setting make buffer names to unique.
;; Adds directory name when same file names exist.
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Turn on recent file mode.
(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 40)

;; ido-mode allows you yo more easily navigate choices.
(ido-mode t)

;; This allows partial atches.
(setq ido-enable-flex-matching t)

;; Turn this behavior off because it's annoying.
(setq ido-use-filename-at-point nil)

;; Don't try to match file across all "work" directories.
;; Only match files in the current directory displayed in the minibuffer.
(setq ido-auto-merge-work-directories-length -1)

;; Includes buffer names of recently open files.
(setq ido-use-virtual-buffers t)

;; This enables ido in all contexts where it could be useful.
(ido-ubiquitous-mode 1)

;; Shows a list of buffers
(global-set-key (kbd "C-x b") 'ibuffer)



;; Enhances M-x to allow easier execution of commands.
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)



;; projectile everywhere
(projectile-global-mode)
