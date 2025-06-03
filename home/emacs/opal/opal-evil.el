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
  (evil-define-key 'normal 'global (kbd "<leader>ff") #'counsel-find-file)
  (evil-define-key 'normal 'global (kbd "<leader>bd") #'kill-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>bk") #'kill-current-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>bi") #'ibuffer)
  (evil-define-key 'normal 'global (kbd "<leader>bf") #'switch-to-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>bp") #'switch-to-prev-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>bn") #'switch-to-next-buffer)
  ;; theme switching
  (evil-define-key 'normal 'global (kbd "<leader>ht") #'counsel-load-theme)
  (evil-mode))

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-setup-minibuffer t)
  (evil-collection-init))

(provide 'opal-evil)
