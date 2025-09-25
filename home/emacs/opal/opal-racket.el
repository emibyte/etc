;;; opal-racket.el --- racket language configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'opal-package)

(use-package racket-mode
  :hook ((racket-mode . lsp-deferred)
         (racket-mode . racket-xp-mode)
         ;; NOTE: sometimes direnv loads too late resulting in the racket executable not being found which is a problem for the two hooks that follow
         (racket-mode . direnv-update-environment)))

;;; TODO: racket-xp-mode, bindings like spc m as prefix for all the racket mode specific bindings pls :3 (use hydra for that also, once i understood how hydra works)
;;; TODO: paredit for all lisps
(provide 'opal-racket)
;;; opal-racket.el ends here
