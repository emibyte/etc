;;; opal-completion --- poison ivy -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'opal-package)

;; Borrowed from system-crafters vid on vertico
(defun opal/minibuffer-backward-kill (arg)
  "Delete up to parent (ARG chars back) when completing a file name in minibuffer."
  (interactive "p")
  (if minibuffer-completing-file-name
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
    (delete-backward-char arg)))

;;; Vertico setup
(use-package vertico
  :bind (:map minibuffer-local-map
              ("<backspace>" . opal/minibuffer-backward-kill)
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous))
  :config
  (setq vertico-cycle t)
  (setq vertico-resize nil)
  :init
  (vertico-mode 1))

(use-package marginalia
  :after vertico
  :config
  (marginalia-mode 1))

(use-package orderless
  :config
  (setq completion-styles '(orderless basic)))

(use-package consult
  :bind (("C-s" . consult-line)))

(use-package embark
  :after vertico
  :bind (("<leader>dj" . embark-dired-jump)))

(use-package embark-consult)

(use-package corfu
  :hook (lsp-completion-mode . opal/corfu-setup-lsp)
  :bind (:map corfu-map
              ("C-j" . corfu-next)
              ("C-k" . corfu-previous)
              ("<tab>" . corfu-insert))
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-auto-prefix 1)
  ;; (lsp-completion-provider :none)
  (corfu-auto-delay 0.25)
  (corfu-min-width 100)
  (corfu-max-width corfu-min-width)
  (corfu-count 14)
  (corfu-scroll-margin 4)
  :init
  (corfu-popupinfo-mode 1)
  (global-corfu-mode 1)
  :config
  (defun opal/corfu-setup-lsp ()
    "Use orderless completion style with lsp-capf instead of the lsp-passthrough."
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))))

;; https://github.com/minad/corfu/discussions/457
(setopt text-mode-ispell-word-completion nil)
(customize-set-variable 'text-mode-ispell-word-completion nil)

(use-package cape
  :hook ((org-mode . opal/cape-capf-setup-org-mode)
         (emacs-lisp-mode . opal/cape-capf-setup-elisp-mode)
         (lsp-completion-mode . opal/cape-capf-setup-lsp-mode)
         )
  :bind
  ("C-c p" . cape-prefix-map)
  :init
  ;; (add-hook 'completion-at-point-functions #'cape-dabbrev)
  ;; (add-hook 'completion-at-point-functions #'cape-file)
  ;; (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-hook 'completion-at-point-functions #'cape-elisp-symbol)
  ;; (add-hook 'completion-at-point-functions #'cape-keyword)
  ;;; org-mode
  (defun opal/cape-capf-setup-org-mode ()
    (dolist (element (list
                      (cape-capf-super #'cape-dict #'cape-dabbrev)))
      (add-to-list 'completion-at-point-functions element)))
  ;;; lsp-mode
  (defun opal/cape-capf-setup-lsp-mode ()
    "Replace the default `lsp-completion-at-point' with its
`cape-capf-buster' version. Also add `cape-file'
backend. Additionally keep `dabbrev' as fallback"
    (setf (elt (cl-member 'lsp-completion-at-point completion-at-point-functions) 0)
          (cape-capf-buster #'lsp-completion-at-point))
    (add-to-list 'completion-at-point-functions #'cape-dabbrev t))
  ;;; elisp-mode
  (defun opal/cape-capf-setup-elisp-mode ()
    "Replace the default `elisp-completion-at-point'
completion-at-point-function. Doing it this way will prevent
disrupting the addition of other capfs (e.g. merely setting the
variable entirely, or adding to list).
Additionally, add `cape-file' as early as possible to the list."
    (setf (elt (cl-member 'elisp-completion-at-point completion-at-point-functions) 0)
          #'elisp-completion-at-point)
    (add-to-list 'completion-at-point-functions #'cape-keyword)
    (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
    ;; I prefer this being early/first in the list
    (add-to-list 'completion-at-point-functions #'cape-file))
  )

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-style '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.9 :scale 1.0
          :background nil))
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
  (kind-icon-blend-background nil)  ; Use midpoint color between foreground and background colors ("blended")?
  (kind-icon-blend-frac 0.08)

  ;; `kind-icon' depends `svg-lib' which creates a cache
  (svg-lib-icons-dir "~/.cache/svg-lib/cache/") ; Change cache dir
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter) ; Enable `kind-icon'

  ;; Add hook to reset cache so the icon colors match my theme
  ;; (add-hook 'kb/themes-hooks #'(lambda () (interactive) (kind-icon-reset-cache)))
  )

(provide 'opal-completion)
;;; opal-completion.el ends here
