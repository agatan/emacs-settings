;; Determine 'user-emacs-directory'
(when load-file-name
  (setq user-emacs-directory (expand-file-name
                              (file-name-directory load-file-name))))


(load (locate-user-emacs-file "bootstrap"))


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


;;;;
;; Lnaguages
;;;;
(load "setup-clojure.el")

(load "setup-common-lisp.el")

(load "setup-c-cpp.el")
