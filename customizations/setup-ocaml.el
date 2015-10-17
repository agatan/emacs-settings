;;;; settings for editting OCaml.

(use-package tuareg
  :mode (("\\.ml" . tuareg-mode))
  :init
  (add-hook 'tuareg-mode-hook
            '(lambda ()
               (local-set-key "\C-c;" 'ocamlspot-query)
               (local-set-key "\C-c:" 'ocamlspot-query-interface)
               (local-set-key "\C-c'" 'ocamlspot-query-uses)
               (local-set-key "\C-c\C-t" 'ocamlspot-type)
               (local-set-key "\C-c\C-i" 'ocamlspot-xtype)
               (local-set-key "\C-c\C-y" 'ocamlspot-type-and-copy)
               (local-set-key "\C-ct" 'caml-types-show-type)
               (local-set-key "\C-cp" 'ocamlspot-pop-jump-stack)))
  :config
  (setq opam-share
        (substring
         (shell-command-to-string "opam config var share 2> /dev/null")
         0 -1))
  (add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
  (require 'utop)
  (require 'ocp-indent)
  (require 'ocamlspot))
