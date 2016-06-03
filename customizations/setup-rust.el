;;;; rust language support
(provide 'misc)
(use-package rust-mode
  :mode (("\\.rs$'" . rust-mode))
  :config
  (progn
    (use-package flycheck-rust)
    (use-package racer)
    (setq racer-rust-src-path "~/rust/rust/src/")
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (add-hook 'racer-mode-hook #'company-mode)))
