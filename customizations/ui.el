;; These customizations change the way emacs looks and disable/enable
;; some user interface elements.

;; Turn off the menu bar at the top of each frame.
(menu-bar-mode -1)

;; Turn off the graphical tool bar at the top of each frame.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Color Themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
(use-package warm-night-theme
  :config
  (load-theme 'warm-night t))

;; increse font size for better readability
(cond
 ((or (eq system-type 'mac) (eq system-type 'darwin))
  (set-face-attribute 'default nil :family "Ricty Diminished" :height 140))
 ((eq system-type 'gnu/linux) (set-face-attribute 'default nil :family "Ricty Diminished" :height 100)))


;; These settings relate to how emacs interacts with your operation system
(setq
 ;; makes killing/yanking interact with the clipboard
 x-select-enable-clipboard t

 ;; I'm not sure what it is.
 x-select-enable-primary t
 save-interprogram-paste-before-kill t
 apropos-do-all t
 mouse-yank-at-point t)

;; No cursor blinking.
(blink-cursor-mode 0)

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; don't pop up font menu
(global-set-key (kbd "s-t") '(lambda () (interactive)))

;; no bel
(setq ring-bell-function 'ignore)

;; Turn off scroll bar
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; show column
(column-number-mode 1)

;; Show trailing white spaces
(setq-default show-trailing-whitespace t)


(use-package rainbow-delimiters)


;; popup windows
(use-package popwin
  :config
  (progn
    (popwin-mode 1)
    (setq display-buffer-function 'popwin:display-buffer)
    (setq popwin:popup-window-position 'bottom)))
