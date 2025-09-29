;;; opal-lsp --- lsp -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'opal-package)

(use-package yasnippet
  :config
  (yas-reload-all)
  :hook ((lsp-mode . yas-minor-mode)))

(setq read-process-output-max (* 1024 1024))

(defun opal/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :custom
  (lsp-completion-provider :none)
  (lsp-lens-enable nil)
  (lsp-prefer-flymake nil)
  (lsp-enable-snippet t)
  (lsp-eldoc-render-all t) ;; minibuffer showing doc
  (lsp-log-io nil)
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . opal/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "<leader>l")
  ;; (setq lsp-session-file "/home/emily/.cache/.lsp-session-v1")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :custom
  (lsp-ui-doc-enable nil) ;; if i enable this i get the extra childframe together with the minibuffer stuff
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-doc-delay 0)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-alignment 'window)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-show-with-mouse t)
  (lsp-flycheck-live-reporting nil)
  :hook (lsp-mode . lsp-ui-mode))

(use-package consult-lsp
  :after lsp-mode
  ;; TODO: bindings for lsp-diag, lsp-symbols, lsp-file-symbols
  )

(provide 'opal-lsp)
;;; opal-lsp.el ends here
