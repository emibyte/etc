;;; opal-theme --- theme -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'opal-package)

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

(use-package ef-themes
  :config
  (load-theme 'doom-shades-of-purple t nil))
  ;; (load-theme 'ef-summer t nil))
  ;; (load-theme 'kaolin-valley-light t nil))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

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
  

(provide 'opal-theme)
;;; opal-theme.el ends here

