;;; opal-elisp.el --- Description -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; TODO: setup some useful bindings like eval and stuff via HYDRA

;; this is probably not how u are supposed to do this, but at least it works for now x.x 
;; also only for elisp for now bcs the lexical binding is kinda mandatory so a file template is needed as opposed to other languages
(defun opal/insert-elisp-default-template ()
  "Automatically insert elisp template if buffer new, empty and not a special buffer."
  (yas-minor-mode)
  (when (and (eq (point-min) (point-max))
             (buffer-file-name)
             (not (buffer-modified-p))
             (not (member (substring (buffer-name) 0 1) '("*" " ")))
    (yas-insert-snippet "__elisp-basic-template"))))

(defun opal/elisp-setup ()
  (outline-minor-mode)
  (opal/insert-elisp-default-template))

(add-hook 'emacs-lisp-mode-hook #'opal/elisp-setup)

(provide 'opal-elisp)
;;; opal-elisp.el ends here
