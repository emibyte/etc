;;; opal-nix --- the language that is very fun and not annoying -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'opal-package)

(use-package nix-prettify-mode
  :config
  (add-to-list 'nix-prettify-special-modes 'eshell-mode)
  (nix-prettify-global-mode))

(use-package nix-mode
  :mode "\\.nix\\'"
  ;;:hook (nix-mode . lsp-deferred)
  )

(provide 'opal-nix)
