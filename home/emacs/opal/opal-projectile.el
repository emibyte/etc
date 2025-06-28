;;; opal-projectile --- project stuff -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'opal-package)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :init
  (when (and (file-directory-p "~/src/")
             (file-directory-p "~/etc/"))
    (setq projectile-project-search-path '("~/src/" "~/etc/")))
;;  (setq projectile-switch-project-action #'projectile-dired)
  )

(evil-define-key 'normal 'global (kbd "<leader>p") 'projectile-command-map)
(evil-define-key 'normal 'global (kbd "<leader>pf") #'consult-projectile-find-file)
(evil-define-key 'normal 'global (kbd "<leader>pwf") #'consult-projectile-find-file-other-window)
(evil-define-key 'normal 'global (kbd "<leader>pp") #'consult-projectile-switch-project)
(evil-define-key 'normal 'global (kbd "<leader>pb") #'consult-project-buffer)
(evil-define-key 'normal 'global (kbd "<leader>pd") #'consult-projectile-find-dir)
(evil-define-key 'normal 'global (kbd "<leader>pr") #'consult-projectile-recentf)

(use-package consult-projectile)

(use-package direnv
  :config
  (direnv-mode))

(provide 'opal-projectile)
;;; opal-projectile.el ends here
