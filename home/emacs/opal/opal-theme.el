;;; opal-theme --- theme -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'opal-package)
(require 'ef-themes)

(set-frame-font "Iosevka Comfy:pixelsize=20")
(set-face-font 'default "Iosevka Comfy:pixelsize=20")

(set-fontset-font
 t 'symbol
 (font-spec
  :family "Noto Color Emoji"
  :size 18
  :weight 'normal
  :width 'normal
  :slant 'normal))

(defun opal/pick-random-ef-dark-theme ()
  "Pick a theme randomly out of the `ef-themes-dark-themes' on startup."
  (let* ((dark-themes ef-themes-dark-themes)
         (length-dark-themes (length dark-themes)))
    (nth (random length-dark-themes) ef-themes-dark-themes)))

(use-package ef-themes
  :config
  (load-theme 'catppuccin t nil))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  ;; :hook (yaml-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        '(
          ("TODO" warning bold)
          ("FIXME" error bold)
          ("REVIEW" font-lock-keyword-face bold)
          ("HACK" font-lock-constant-face bold)
          ("DEPRECATED" font-lock-doc-face bold)
          ("NOTE" success bold)
          ("BUG" error bold)
          ("XXX" font-lock-constant-face-bold))))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; https://emacsredux.com/blog/2020/11/21/disable-global-hl-line-mode-for-specific-modes/
(add-hook 'vterm-mode-hook (lambda () (setq-local global-hl-line-mode nil)))

;;; modeline
(column-number-mode t)
(defvar lsp-modeline--code-actions-string nil)

(setq-default mode-line-format
              '("%e"
                (:propertize " " display (raise +0.4)) ;; padding top
                (:propertize " " display (raise -0.4)) ;; padding bottom

                (:propertize "λ " face font-lock-comment-face)
                mode-line-frame-identification
                mode-line-buffer-identification

                (:eval (when-let (vc vc-mode)
                         (list (propertize "   " 'face 'font-lock-comment-face)
                               ;; Truncate branch name to 50 characters
                               (propertize (truncate-string-to-width
                                            (substring vc 5) 50)
                                           'face 'font-lock-comment-face))))

                (:eval (propertize
                        " " 'display
                        `((space :align-to
                                 (-  (+ right right-fringe right-margin)
                                     ,(+ 3
                                         (string-width (or lsp-modeline--code-actions-string ""))
                                         (string-width "%4l:3%c")))))))

                (:eval (or lsp-mode-line--code-actions-string ""))

                (:propertize "4l:3%c" face mode-line-buffer-id)))

(provide 'opal-theme)
;;; opal-theme.el ends here

