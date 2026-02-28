;;; opal-completion --- poison ivy -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'opal-package)
(require 'corfu)
(require 'yasnippet)

;;; yasnippet
(use-package yasnippet
  :hook (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets :defer)

(defun start/corfu-yas-tab-handler ()
  "Prioritize corfu over yasnippet when yasnippet is active."
  (interactive)
  ;; There is no direct way to get if corfu is currently displayed so we watch the completion index
  (if (> corfu--index -1)
      (corfu-complete)
    (yas-next-field-or-maybe-expand)
    ))
(use-package emacs
  :after (yasnippet corfu)
  :bind
  (:map yas-keymap
        ("TAB" . start/corfu-yas-tab-handler)))

;;; Vertico setup
(use-package vertico
  :bind (:map minibuffer-local-map
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
  (setq completion-styles '(orderless basic partial-completion)))

(use-package consult
  :bind (("C-s" . consult-line)))

(use-package embark
  :after vertico
  :bind (("<leader>dj" . embark-dired-jump)))

(use-package embark-consult)

(use-package corfu
  :bind (:map corfu-map
              ("C-j" . corfu-next)
              ("C-k" . corfu-previous)
              ("<tab>" . corfu-insert))
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-popupinfo-mode t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.5)
  (completion-ignore-case t)
  (corfu-scroll-margin 5)
  ;; (corfu-count 14)
  (corfu-min-width 200)
  :init
  (global-corfu-mode 1))

;; https://github.com/minad/corfu/discussions/457
(setopt text-mode-ispell-word-completion nil)
(customize-set-variable 'text-mode-ispell-word-completion nil)

(use-package yasnippet-capf :defer)

(defun start/setup-capfs ()
  "Configure completion backends."
  (let ((merge-backends (list
                         #'cape-keyword         ;; Keyword completion
                         ;; #'cape-abbrev       ;; Complete abbreviation
                         #'cape-dabbrev         ;; Complete word from current buffers
                         ;; #'cape-line         ;; Complete entire line from current buffer
                         ;; #'cape-history      ;; Complete from Eshell, Comint or minibuffer history
                         ;; #'cape-dict         ;; Dictionary completion (Needs Dictionary file installed)
                         ;; #'cape-tex          ;; Complete Unicode char from TeX command, e.g. \hbar
                         ;; #'cape-sgml         ;; Complete Unicode char from SGML entity, e.g., &alpha
                         ;; #'cape-rfc1345      ;; Complete Unicode char using RFC 1345 mnemonics
                         ;; #'snippy-capf       ;; Vscode Snippets (Snippy needs to be installed)
                         #'yasnippet-capf       ;; Yasnippet snippets
                         ))
        (seperate-backends (list
                            #'cape-file ;; Path completion
                            #'cape-elisp-block ;; Complete elisp in Org or Markdown mode
                            )))
    ;; Remove keyword completion in git commits
    (when (derived-mode-p 'git-commit-mode)
      (setq merge-backends (remq #'cape-keyword merge-backends)))

    ;; Add Elisp symbols only in Elisp modes
    (when (derived-mode-p 'emacs-lisp-mode 'ielm-mode)
      (setq merge-backends (cons #'cape-elisp-symbol merge-backends))) ;; Emacs Lisp code (functions, variables)

    ;; Add Eglot to the front of the list if it's active
    (when (bound-and-true-p eglot--managed-mode)
      (setq merge-backends (cons #'eglot-completion-at-point merge-backends)))

    ;; Create the super-capf and set it buffer-locally
    (setq-local completion-at-point-functions
                (append
                 seperate-backends
                 (list (apply #'cape-capf-super merge-backends)))
                )))

(use-package cape
  :after (corfu)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.

  ;; Seperate function needed, because we use setq-local (everything is replaced)
  (add-hook 'eglot-managed-mode-hook #'start/setup-capfs)
  (add-hook 'prog-mode-hook #'start/setup-capfs)
  (add-hook 'text-mode-hook #'start/setup-capfs))

(use-package nerd-icons-corfu
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(provide 'opal-completion)
;;; opal-completion.el ends here
