;;; opal-projectile --- project stuff -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'opal-package)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'vertico))
  :init
  (when (and (file-directory-p "~/src/")
             (file-directory-p "~/etc/"))
    (setq projectile-project-search-path '("~/src/" "~/etc/")))
;;  (setq projectile-switch-project-action #'projectile-dired)
  )

(evil-define-key 'normal 'global (kbd "<leader>p") 'projectile-command-map)
(evil-define-key 'normal 'global (kbd "<leader>pf") #'consult-projectile-find-file)
(evil-define-key 'normal 'global (kbd "<leader>pp") #'consult-projectile-switch-project)
(evil-define-key 'normal 'global (kbd "<leader>pb") #'consult-project-buffer)

(use-package consult-projectile)

(use-package direnv
  :config
  (direnv-mode))

(provide 'opal-projectile)
