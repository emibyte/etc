;;; opal-completion --- poison ivy -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'opal-package)

;; Borrowed from system-crafters vid on vertico
(defun opal/minibuffer-backward-kill (arg)
  "When minibuffer is completing a file name delete up to parent
folder, otherwise delete a character backward."
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
  :config
  (marginalia-mode 1))

(use-package orderless
  :config
  (setq completion-styles '(orderless basic)))

(use-package consult
  :bind (("C-s" . consult-line)))

(use-package embark)

(use-package embark-consult)

(provide 'opal-completion)
