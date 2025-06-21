;;; opal-vc.el --- version control magit -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'opal-package)

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(evil-define-key 'normal 'global (kbd "<leader>gs") #'magit-status)

(provide 'opal-vc)
;;; opal-vc.el ends here
