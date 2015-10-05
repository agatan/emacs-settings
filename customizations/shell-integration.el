;; Set up exec-shell-from-shell

(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :config
    (progn
      (exec-path-from-shell-initialize)
      (exec-path-from-shell-copy-envs '("PATH")))))
