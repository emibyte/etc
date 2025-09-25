;;; python.el --- python setup -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'opal-package)

;; TODO: uv-mode

(use-package lsp-python-ms
  :hook ((python-mode . opal/setup-python)
         (python-mode . direnv-update-environment))
  :init
  (setq lsp-python-ms-executable (executable-find "python-language-server"))
  (defun opal/setup-python ()
    (lsp-deferred))
  )

(provide 'opal-python)
;;; opal-python.el ends here
