;;; opal-workspaces.el --- workspaces, switching and reloading -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'opal-package)

(use-package perspective
  :custom
  (persp-mode-prefix-key (kbd "C-c C-p"))
  :config
  (evil-define-key 'normal 'global (kbd "<leader>xb") #'persp-list-buffers)
  :init
  (persp-mode))

(provide 'opal-workspaces)
;;; opal-workspaces.el ends here
