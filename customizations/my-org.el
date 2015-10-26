;;;; editting org files and agenda.
(use-package org
  :bind (("C-c a t" . org-agenda))
  :config
  (progn
    (setq org-directory "~/Dropbox/org/")
    (setq org-agenda-files (list org-directory))
    (setq org-src-fontify-natively t)))
(use-package open-junk-file
  :bind (("C-x j" . open-junk-file))
  :config
  (progn
    (setq open-junk-file-format "~/Dropbox/org/%Y-%m%d-memo.")))
