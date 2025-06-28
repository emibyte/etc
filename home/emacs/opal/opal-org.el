;;; opal-org --- some orgy stuff -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'opal-package)

(defun opal/org-mode-setup ()
  (org-indent-mode)
  (auto-fill-mode 0)
  (visual-line-mode 1))

(defun opal/org-mode-font-setup ()
  "Set faces for heading levels."
  (dolist (face '((org-level-1 . 1.3)
                  (org-level-2 . 1.25)
                  (org-level-3 . 1.2)
                  (org-level-4 . 1.15)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face))))

(defun opal/org-mode-visual-fill-col ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package org
  :custom
  (org-directory "~/notes")
  (org-default-notes-file "~/notes/scratch.org")
  (org-agenda-files '("~/notes"))
  :hook
  (org-mode . opal/org-mode-setup)
  (org-mode . opal/org-mode-font-setup)
  :config
  (setq org-hide-emphasis-markers nil
        org-ellipsis " ▾") ; replace the ... at end of headings that contain sth
  (opal/org-mode-font-setup)
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

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  ;; in case i want it simpler
  ;; :custom
  ;; (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●"))
  )

(use-package visual-fill-column
  :hook (org-mode . opal/org-mode-visual-fill-col))

(provide 'opal-org)
;;; opal-org.el ends here
