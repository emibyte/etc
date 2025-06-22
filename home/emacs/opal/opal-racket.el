;;; opal-racket.el --- racket language configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'opal-package)

;; TODO: racket-xp-mode, bindings like spc m as prefix for all the racket mode specific bindings pls :3
;; TODO: paredit for all lisps
;; TODO: theres still some issues where theres no syntax highlighting if the first buffer i open in emacs is a racket buffer
(use-package racket-mode
  :hook (racket-mode . lsp-deferred)
  :hook (racket-mode . racket-xp-mode))

(provide 'opal-racket)
;;; opal-racket.el ends here
