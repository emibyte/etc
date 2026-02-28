;;; opal-eglot.el --- l(i)sp(y) stuff -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'opal-package)

(use-package eglot
  :hook ((c-mode c++-mode python-mode nix-mode)
         . eglot-ensure)
  :custom
  ;; Good defaults
  (eglot-events-buffer-size 0) ;; No event buffers (LSP server logs)
  (eglot-autoshutdown t)       ;; Shutdown unused servers.
  (eglot-report-progress nil)  ;; Disable LSP server logs (Don't show lsp messages at the bottom, java)
  (eglot-inlay-hints-mode t)   ;; Inlay hints for now
  ;; Manual lsp servers
  ;;:config
  ;;(add-to-list 'eglot-server-programs
  ;;             `(lua-mode . ("PATH_TO_THE_LSP_FOLDER/bin/lua-language-server" "-lsp"))) ;; Adds our lua lsp server to eglot's server list
  )

(defun opal/update-eldoc-box-border ()
  (interactive)
  (let ((color (face-attribute 'font-lock-keyword-face :foreground nil t)))
    (when (stringp color)
      (set-face-attribute 'eldoc-box-border nil :background color))))

(use-package eldoc-box
  :init
  (setq eldoc-box-frame-parameters '((left . -1)
                                     (top . -1)
                                     (width  . 0)
                                     (height  . 0)

                                     (no-accept-focus . t)
                                     (no-focus-on-map . t)
                                     (min-width  . 0)
                                     (min-height  . 0)
                                     (internal-border-width . 2)
                                     (vertical-scroll-bars . nil)
                                     (horizontal-scroll-bars . nil)
                                     (right-fringe . 9)
                                     (left-fringe . 9)
                                     (menu-bar-lines . 0)
                                     (tool-bar-lines . 0)
                                     (line-spacing . 0)
                                     (unsplittable . t)
                                     (undecorated . t)
                                     (visibility . nil)
                                     (mouse-wheel-frame . nil)
                                     (no-other-frame . t)
                                     (cursor-type . nil)
                                     (inhibit-double-buffering . t)
                                     (drag-internal-border . t)
                                     (no-special-glyphs . t)
                                     (desktop-dont-save . t)
                                     (tab-bar-lines . 0)
                                     (tab-bar-lines-keep-state . 1)))
  :custom
  (eldoc-idle-delay 0.0)
  :config
  ;; (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t) ;; if i want the popup on hover
  (add-hook 'eldoc-box-buffer-setup-hook #'eldoc-box-prettify-ts-errors 0 t)
  (evil-define-key 'normal 'global "K" #'eldoc-box-help-at-point)
  (with-eval-after-load 'eglot
    (evil-define-key 'normal eglot-mode-map "K" #'eldoc-box-help-at-point))
  (opal/update-eldoc-box-border)
  )


(provide 'opal-eglot)
;;; opal-eglot.el ends here
