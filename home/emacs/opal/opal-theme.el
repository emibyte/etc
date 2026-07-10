;;; opal-theme --- theme -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'opal-package)
(require 'ef-themes)

;; (set-frame-font "Iosevka Comfy:pixelsize=18")
;; (set-face-font 'default "Iosevka Comfy:pixelsize=18")

(set-frame-font "Maple Mono NF:pixelsize=18")
(set-face-font 'default "Maple Mono NF:pixelsize=18")

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
  (interactive)
  (let* ((dark-themes ef-themes-dark-themes)
         (length-dark-themes (length dark-themes)))
    (nth (random length-dark-themes) ef-themes-dark-themes)))

(use-package ef-themes
  :config
  (load-theme 'doric-mermaid t nil))
  ;; (load-theme 'kaolin-mono-dark t nil))
  ;; (load-theme 'ef-autumn t nil))
  ;; (load-theme 'catppuccin t nil))
  ;; (load-theme 'stimmung-themes-dark t nil))
  ;; (load-theme 'ef-tritanopia-dark t nil))
  ;; (load-theme 'gruber-darker t nil))

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

(defvar home-dir (getenv "HOME"))
(defun opal/replace-home (dir)
  (if (file-remote-p dir) dir
    (s-replace home-dir "~" dir)))

;;; modeline
;; (use-package doom-modeline
;;   :custom
;;   (doom-modeline-height 25) ;; Set modeline height
;;   (doom-modeline-buffer-encoding nil)
;;   (doom-modeline-lsp nil)
;;   (doom-modeline-check nil)
;;   :hook (after-init . doom-modeline-mode))

;; taken from lcolonq's stream emacs config
(defun mode-line-render (left right)
  "Return a string of `window-width' length containing LEFT and RIGHT aligned respectively."
  (let* ((available-width (- (window-width) (length left) 3)))
    (format (format " %%s %%%ds " available-width) left right)))
(setq-default
 mode-line-format
 `((:eval
    (mode-line-render
     (concat
      (propertize (format-mode-line "λ ") 'face 'bold)
      (propertize (format-mode-line (buffer-name)) 'face 'bold)
      (format-mode-line evil-mode-line-tag)
      "- "
      (format-mode-line mode-name)
      " - "
      (opal/replace-home default-directory))
     (format-mode-line '(line-number-mode (" line %l" (column-number-mode " column %c"))))))))

;;; nerd-icons
(use-package nerd-icons :defer)

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package ligature
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(provide 'opal-theme)
;;; opal-theme.el ends here

