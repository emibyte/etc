;;; opal-projectile --- project stuff -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'opal-package)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :init
  (when (and (file-directory-p "~/src/")
             (file-directory-p "~/etc/"))
    (setq projectile-project-search-path '("~/src/" "~/etc/")))
;;  (setq projectile-switch-project-action #'projectile-dired)
  )

(evil-define-key 'normal 'global (kbd "<leader>p") 'projectile-command-map)

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package direnv
  :config
  (direnv-mode))

(provide 'opal-projectile)
