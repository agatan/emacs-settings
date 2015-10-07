;;;; settings for editting c and c++ code.
(add-hook 'c-mode-common-hook
          (lambda ()
            (setq c-default-style "k&r")
            (setq indent-tabs-mode nil)
            (setq c-basic-offset 2)))

(add-to-list 'auto-mode-alist '("\\.hpp$'" . c++-mode))

(use-package auto-complete-c-headers
  :config
  (progn
    (add-hook 'c++-mode-hook (lambda () (push 'ac-source-c-headers ac-sources)))
    (add-hook 'c-mode-hook (lambda () (push 'ac-source-c-headers ac-sources)))))

(use-package helm-make)

(defun my/ac-cpp-mode-setup ()
  (setq ac-sources (cons 'ac-source-clang-async ac-sources))
  (setq ac-clang-cflags '("-std=c++1z"))
  (ac-clang-launch-completion-process))

(use-package auto-complete-clang-async
  :config
  (add-hook 'c++-mode-hook 'my/ac-cpp-mode-setup))
