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

;; The packages I want installed.
(defvar my-packages
  '(;; makes handling lisp expressions much
    paredit

    ;; auto completion for everythig
    auto-complete

    ;; Syntax checker for many languages
    flycheck

    ;; Easy to open shell
    shell-pop

    ;; Input Japanese
    ddskk

    ;; allow ido usage in as many contexts as possible.
    ido-ubiquitous

    ;; Enhances M-x to allow easier execution of commands.
    smex

    ;; project navigation
    projectile

    ;; colorful parenthesis matching
    rainbow-delimiters

    ;; edit html tags like sexps
    tagedit

    ;; git integration
    magit))

;; ON OS X, an Emacs instance started from graphical user
;; interface will have a different environment than a shell.
(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))


;; Install packages
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


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

;; Add a directory to our load path
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Shell path settings
(load "shell-integration.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs lokks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard to categorize other categories.
(load "misc.el")

;; Editting emacs-list
(load "elisp-editting.el")


;; skk settings
(load "my-skk.el")
