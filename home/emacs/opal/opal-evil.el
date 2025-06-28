;;; opal-evil --- malicious -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'opal-package)

(use-package undo-tree
  :custom
  (undo-tree-auto-save-history nil)
  :config
  (global-undo-tree-mode))

(use-package evil
  :custom
  ;; (evil-want-minibuffer t) ;; can't get it to play nice with the ivy minibuffers
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-want-C-u-scroll t)
  (evil-undo-system 'undo-tree)
  :config
  (evil-set-leader 'normal (kbd "SPC"))
  ;; buffer keybinds
  (evil-define-key 'normal 'global (kbd "<leader>ff") #'find-file)
  (evil-define-key 'normal 'global (kbd "<leader>bd") #'kill-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>bk") #'kill-current-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>bi") #'ibuffer)
  ;;(evil-define-key 'normal 'global (kbd "<leader>bb") #'switch-to-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>bb") #'consult-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>bp") #'switch-to-prev-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>bn") #'switch-to-next-buffer)
  ;; theme switching
  (evil-define-key 'normal 'global (kbd "<leader>ht") #'consult-theme)
  ;; compile command
  (evil-define-key 'normal 'global (kbd "<leader>cc") #'compile)
  ;; searching
  (evil-define-key 'normal 'global (kbd "<leader>sg") #'consult-grep)
  (evil-define-key 'normal 'global (kbd "<leader>sr") #'consult-ripgrep)
  (evil-define-key 'normal 'global (kbd "<leader>sf") #'consult-find)
  (evil-define-key 'normal 'global (kbd "<leader>so") #'consult-outline)
  (evil-define-key 'normal 'global (kbd "<leader>sl") #'consult-line)

  (evil-define-key 'normal 'global (kbd "<leader>hv") #'describe-variable)
  (evil-define-key 'normal 'global (kbd "<leader>hf") #'describe-function)
  (evil-mode))

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-setup-minibuffer t)
  (setq evil-collection-key-blacklist '("SPC")) ;; little (not-really)fix to make my leader key be available in stuff like dired as well
  (evil-collection-init))

(use-package evil-nerd-commenter
  :config
  (evil-define-key 'normal 'global (kbd "gc") #'evilnc-comment-or-uncomment-lines))

(provide 'opal-evil)
;;; opal-evil.el ends here
