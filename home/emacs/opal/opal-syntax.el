;;; opal-syntax.el --- syntax checking with flycheck -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'opal-package)
(require 'dash)
(require 'flymake)
(require 'quick-peek)

(use-package flycheck
  :custom
  (flycheck-display-errors-function (lambda (errors) (ignore errors) nil))
  (eldoc-idle-delay 0.0)
  (flycheck-display-errors-delay 0.1)
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-check-syntax-automatically '(save mode-enabled))
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package quick-peek)

(use-package flymake
  :config
  (defun flymake-eldoc-function (report-doc &rest _)
    "Document diagnostics at point.
Intended for `eldoc-documentation-functions' (which see)."
    (let ((diags (flymake-diagnostics (point))))
      (when diags
        (funcall report-doc
                 (mapconcat #'flymake-diagnostic-text diags "\n")
                 :sort 'error)))))

(use-package eldoc
  :custom
  (eldoc-idle-delay 0.1)
  (eldoc-echo-area-prefer-doc-buffer t)
  :config
  (defun opal/eldoc-display-quick-peek (docs interactive)
    "Display DOCS in a `quick-peek' popup."
    (when docs
      (--each docs
        (if (eq (plist-get (cdr it) :sort) 'error)
            (let ((msg
                   (with-current-buffer (eldoc--format-doc-buffer (list it))
                     (buffer-string))))
              (quick-peek-show (format "%s" msg)))
          (eldoc-display-in-echo-area (list it) interactive)))))
  (setq eldoc-documentation-strategy #'eldoc-documentation-compose)
  (setq eldoc-display-functions '(eldoc-display-in-echo-area eldoc-display-in-buffer)))

(provide 'opal-syntax)
;;; opal-syntax.el ends here
