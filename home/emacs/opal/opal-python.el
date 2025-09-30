;;; python.el --- python setup -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'opal-package)

;; TODO: uv-mode

(use-package lsp-pyright
  :custom
  (lsp-pyright-langserver-command "basedpyright") ;; or basedpyright
  :hook ((python-mode . (lambda ()
                          (require 'lsp-pyright)
                          ;; (direnv-update-environment)
                          (lsp)))))

(provide 'opal-python)
;;; opal-python.el ends here
