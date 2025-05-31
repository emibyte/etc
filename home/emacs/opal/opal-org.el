;;; opal-org --- some orgy stuff -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package org
  :custom
  (org-directory "~/notes")
  (org-default-notes-file "~/notes/scratch.org")
  (org-agenda-files '("~/notes")))

(setq org-startup-folded 'content)

(provide 'opal-org)
