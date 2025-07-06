;;; opal-workspaces.el --- workspaces, switching and reloading -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'opal-package)

(use-package perspective
  :hook
  ('kill-emacs . #'persp-state-save)
  :bind
  (("<leader>ql" . #'persp-state-load))
  :custom
  (persp-mode-prefix-key (kbd "<leader><tab>"))
  (persp-state-default-file "~/.config/emacs/transient/persp-state")
  :config
  (evil-define-key 'normal 'global (kbd "<leader>xb") #'persp-list-buffers)
  :init
  (persp-mode))

(provide 'opal-workspaces)
;;; opal-workspaces.el ends here
