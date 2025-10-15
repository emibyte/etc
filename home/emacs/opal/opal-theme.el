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

(defun opal/pick-random-ef-theme ()
  "Pick a theme randomly out of the `ef-themes-collection' on startup."
  (nth (random (length ef-themes-collection)) ef-themes-collection))

(use-package ef-themes
  :config
  (load-theme (opal/pick-random-ef-theme) t nil))
  ;; (load-theme 'ef-maris-dark t nil))
  ;; (load-theme 'doom-moonlight t nil))
  ;; (load-theme 'stimmung-themes-dark t nil))
  ;; (load-theme 'ef-rosa t nil))
  ;; (load-theme 'kaolin-shiva t nil))
  ;; (load-theme 'kaolin-valley-light t nil))
  ;; (load-theme 'doom-pine t nil))
  ;; (load-theme 'stimmung-themes-dark t nil))

(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  (setq nerd-icons-scale-factor 1.1))

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
  

(provide 'opal-theme)
;;; opal-theme.el ends here

