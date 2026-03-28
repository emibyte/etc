;;; opal-go.el --- go to the polls -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'opal-package)

(use-package go-mode
  :hook (go-mode . (lambda ()
                     (add-hook 'before-save-hook #'eglot-format-buffer nil t))))

;; (use-package go-ts-mode
;;   :hook (go-ts-mode . (lambda ()
;;                         (add-hook 'before-save-hook #'eglot-format-buffer nil t))))


(provide 'opal-go)
;;; opal-go.el ends here
