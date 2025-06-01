;;; opal-org --- some orgy stuff -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package org
  :custom
  (org-directory "~/notes")
  (org-default-notes-file "~/notes/scratch.org")
  (org-agenda-files '("~/notes"))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (gnuplot . t)
     (latex . t)
     (python . t)
     (C . t)
     (shell . t)
     (sqlite . t)))
  )

(evil-define-key 'normal org-mode-map
  (kbd "RET") 'org-toggle-checkbox)


(setq org-startup-folded 'content)

(provide 'opal-org)
