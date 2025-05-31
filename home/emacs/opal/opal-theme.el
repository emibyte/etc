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

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(provide 'opal-theme)

