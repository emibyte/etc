;;; python.el --- python setup -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'opal-package)

;; TODO: uv-mode
(defun opal/setup-python ()
  (require 'lsp-pyright)
  (direnv-update-environment)
  (lsp))

(use-package lsp-pyright
  :hook ((python-mode . opal/setup-python))
  :custom
  (lsp-pyright-langserver-command "pyright") ;; or basedpyright
  )

(provide 'opal-python)
;;; opal-python.el ends here
