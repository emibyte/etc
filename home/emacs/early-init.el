;;; early-init.el --- we do some early init-ing -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq
  gc-cons-threshold (* 1024 1024 128)
  gc-cons-percentage 1.0)

(add-hook 'emacs-startup-hook (lambda ()
                                (setq gc-cons-threshold (* 1024 1024 2)
                                      gc-cons-percentage 0.2)))

(setq read-process-output-max (* 1024 1024))

(defvar last-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'after-init-hook
          (lambda ()
            (setq file-name-handler-alist last-file-name-handler-alist)))

(setq menu-bar-mode nil)                           ;; Disable the menu bar
(setq tool-bar-mode nil)                           ;; Disable the tool bar
(push '(vertical-scroll-bars) default-frame-alist) ;; Disable the scroll bar

(provide 'early-init)
;;; early-init.el ends here
