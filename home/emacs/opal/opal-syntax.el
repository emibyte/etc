;;; opal-syntax.el --- syntax checking with flycheck -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; TODO: still have to fix this

(require 'opal-package)

(use-package sideline-flymake
  :hook (flymake-mode . sideline-mode)
  :init
  (setq sideline-flymake-display-mode 'line) ;; Show errors on the current line
  (setq sideline-backends-right '(sideline-flymake))
  :config
  ;; NOTE(emi): there's a problem where eglot errors seem to contain nil sometimes so split-string will fail
  ;;            and it will cause the errors to not display (should probably report but sideline-flymake report
  ;;            looks kinda dead)
  (defun sideline-flymake--show-errors (callback &rest _)
    "Execute CALLBACK to display with sideline."
    (when flymake-mode
      (when-let* ((errors (sideline-flymake--get-errors)))
        (dolist (err errors)
          (when-let* ((text (flymake-diagnostic-text err))  ; guard nil
                      (lines (split-string text "\n"))
                      (lines (butlast lines (- (length lines) sideline-flymake-max-lines)))
                      (text (mapconcat #'identity lines "\n"))
                      (type (flymake-diagnostic-type err))
                      (type (sideline-flymake--get-level type))
                      (backend (flymake-diagnostic-backend err))
                      (face (pcase type
                              (`error   'sideline-flymake-error)
                              (`warning 'sideline-flymake-warning)
                              (`note    'sideline-flymake-note)))
                      (prefix (pcase type
                                (`error   sideline-flymake-error-prefix)
                                (`warning sideline-flymake-warning-prefix)
                                (`note    sideline-flymake-note-prefix)))
                      (text (concat prefix text)))
            (when sideline-flymake-show-backend-name
              (setq text (format "%s (%s)" text backend)))
            (add-face-text-property 0 (length text) face nil text)
            (funcall callback (list text)))))))
  )

(provide 'opal-syntax)
;;; opal-syntax.el ends here
