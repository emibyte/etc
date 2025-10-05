;;; opal-c.el --- C (and eventually C++) -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'opal-package)

(defun opal/c-setup ()
  "Setting up editing in a C Buffer."
 (c-set-style "linux")
 (electric-pair-mode 1)
 (setq-local indent-tabs-mode nil)
 (setq-local tab-width 2)
 (setq-local c-basic-offset 2)
 (direnv-update-directory-environment)
 (lsp-deferred))

(use-package c-mode
  :hook (c-mode . opal/c-setup))

(provide 'opal-c)
;;; opal-c.el ends here
