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
  (load-theme 'ef-summer t nil))
;; (load-theme 'doom-palenight t nil)

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; NOTE: not sure about whether i want this, kinda helps, but also so much clutter
;; (use-package which-key
;;   :init (which-key-mode)
;;   :diminish which-key-mode
;;   :config
;;   (setq which-key-idle-delay 0.3))
  

(provide 'opal-theme)

