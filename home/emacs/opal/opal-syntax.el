;;; opal-syntax.el --- syntax checking with flycheck -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; TODO: still have to fix this

(require 'opal-package)

(use-package sideline-flymake
  :hook (flymake-mode . sideline-mode)
  :custom
  (sideline-flymake-display-mode 'line) ;; Show errors on the current line
  (sideline-backends-right '(sideline-flymake)))

(provide 'opal-syntax)
;;; opal-syntax.el ends here
