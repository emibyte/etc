;;; init --- initialization -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defvar eln-cache-path
  (format "/home/%s/.cache/eln-cache" user-login-name))

(setcar native-comp-eln-load-path eln-cache-path) ;; eln-cache in .cache dir instead of inside config

(setq
  gc-cons-threshold 402653184
  gc-cons-percentage 0.6)

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ;; literally doesn't do anything and idk why :c
;; (setq inhibit-startup-echo-area-message "emily")

(setq-default
  indent-tabs-mode nil ;; default to indenting with spaces
  bidi-display-reordering nil
  cursor-in-non-selected-windows nil)

(setq
  custom-file "/dev/null" ;; Don't save customizations, just delete them
  shell-file-name "/run/current-system/sw/bin/zsh" ;; zsh location on nixos
  ;; initial-major-mode 'eshell-mode
  make-backup-files nil
  visible-cursor nil
  make-backup-files nil ;; Don't pollute folders with backups
  auto-save-default nil ;; Don't auto-save
  confirm-kill-emacs 'y-or-n-p ;; Ask before exiting Emacs
  initial-scratch-message nil ;; Don't print a bunch of text in scratch buffer
  inhibit-startup-message t ;; Don't show the default emacs splash screen
  sentence-end-double-space nil
  )

(menu-bar-mode -1) ;; Don't display menu bar
(tool-bar-mode -1) ;; Don't display tool bar
(scroll-bar-mode -1) ;; Don't display scroll bar
(blink-cursor-mode -1) ;; No cursor blinking
(tooltip-mode -1) ;; Display tooltips in echo area instead of a popup
;; (show-paren-mode -1) ;; Don't highlight parentheses
(global-hl-line-mode) ;; Highlight current line in all buffers

(column-number-mode) ;; Display column number in modeline
(recentf-mode) ;; Recording recently visited files

(add-to-list 'load-path "~/.config/emacs/opal")
(require 'opal-hydra)
(require 'opal-evil)
(require 'opal-vc)
(require 'opal-syntax)
(require 'opal-theme)
(require 'opal-completion)
(require 'opal-projectile)
(require 'opal-help)
(require 'opal-workspaces)

(require 'opal-lsp)
(require 'opal-org)
(require 'opal-elisp)
(require 'opal-nix)
(require 'opal-racket)

;; Re-enable garbage collection after startup
(setq
 gc-cons-threshold 16777216
 gc-cons-percentage 0.1)

(provide 'init)
;;; init.el ends here
