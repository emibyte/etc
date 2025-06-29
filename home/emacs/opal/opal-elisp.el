;;; opal-elisp.el --- Description -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; TODO: setup some useful bindings like eval and stuff via HYDRA

(defun opal/insert-elisp-default-template ()
  "Insert elisp template if buffer new, empty and not a special buffer."
  (yas-minor-mode)
  (when (and (eq (point-min) (point-max))
             (buffer-file-name)
             (not (buffer-modified-p))
             (not (member (substring (buffer-name) 0 1) '("*" " "))))
    (yas-insert-snippet "__elisp-basic-template")))

(defun opal/elisp-setup ()
  "Setup for elisp-mode."
  (outline-minor-mode)
  (opal/insert-elisp-default-template))

(add-hook 'emacs-lisp-mode-hook #'opal/elisp-setup)

(provide 'opal-elisp)
;;; opal-elisp.el ends here
