;;;; bootstrap.el

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

