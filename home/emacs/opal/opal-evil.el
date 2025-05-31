;;; opal-evil --- malicious -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'opal-package)

(use-package undo-tree
  :config
  (global-undo-tree-mode))

(use-package evil
  :custom
  (evil-want-minibuffer t)
  (evil-undo-system 'undo-tree)
  :config
  (evil-mode))

(provide 'opal-evil)
