;;; opal-syntax.el --- syntax checking with flycheck -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'opal-package)

(use-package flycheck
  :custom
  (eldoc-idle-delay 0.0)
  (flycheck-display-errors-delay 0.1)
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-check-syntax-automatically '(save mode-enabled))
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package quick-peek)

(add-to-list 'load-path "~/src/flycheck-inline")
(use-package flycheck-inline
  :config
  (setq flycheck-inline-display-function
        (lambda (msg &optional pos err)
          (ignore err)
          (set-text-properties 0 (length msg) nil msg)
          (let* ((ov (quick-peek-overlay-ensure-at pos))
                 (contents (quick-peek-overlay-contents ov)))
            (setf (quick-peek-overlay-contents ov)
                  (concat contents (when contents "\n") msg))
            (quick-peek-update ov)))
        flycheck-inline-clear-function #'quick-peek-hide)
  (global-flycheck-inline-mode))

(provide 'opal-syntax)
;;; opal-syntax.el ends here
