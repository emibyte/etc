;;; init --- initialization -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'autorevert)
(require 'recentf)

;;; initialization stuff
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
  custom-file "/dev/null"                          ;; Don't save customizations, just delete them
  shell-file-name "/run/current-system/sw/bin/zsh" ;; zsh location on nixos
  ;; initial-major-mode 'eshell-mode
  make-backup-files nil
  visible-cursor nil
  make-backup-files nil                  ;; Don't pollute folders with backups
  auto-save-default nil                  ;; Don't auto-save
  confirm-kill-emacs 'y-or-n-p           ;; Ask before exiting Emacs
  initial-scratch-message nil            ;; Don't print a bunch of text in scratch buffer
  inhibit-startup-message t              ;; Don't show the default emacs splash screen
  sentence-end-double-space nil

  auto-revert-interval 1                 ;; Refresh buffers fast
  auto-revert-verbose          nil       ;; Don't notify me about reverts
  global-auto-revert-non-file-buffers t  ;; Revert Dired and other buffers
  echo-keystrokes              0.1       ;; Show keystrokes fast
  frame-inhibit-implied-resize 1         ;; Don't resize frame implicitly
  sentence-end-double-space    nil       ;; No double spaces
  recentf-max-saved-items      1000      ;; Show more recent files
  use-short-answers            t         ;; 'y'/'n' instead of 'yes'/'no' etc.
  save-interprogram-paste-before-kill t  ;; Save copies between programs
  history-length               25)        ;; Only save the last 25 minibuffer prompts

(setq-default tab-width 4)               ;; Smaller tabs

(menu-bar-mode -1)                       ;; Don't display menu bar
(tool-bar-mode -1)                       ;; Don't display tool bar
(scroll-bar-mode -1)                     ;; Don't display scroll bar
(blink-cursor-mode -1)                   ;; No cursor blinking
(tooltip-mode -1)                        ;; Display tooltips in echo area instead of a popup
;; (show-paren-mode -1)                  ;; Don't highlight parentheses
(global-hl-line-mode)                    ;; Highlight current line in all buffers

(column-number-mode)                     ;; Display column number in modeline
(recentf-mode)                           ;; Recording recently visited files

(delete-selection-mode   t) ;; Replace selected text when yanking
(global-so-long-mode     t) ;; Mitigate performance for long lines
(global-visual-line-mode t) ;; Break lines instead of truncating them
(global-auto-revert-mode t) ;; Revert buffers automatically when they change
(savehist-mode           t) ;; Remember minibuffer prompt history
(save-place-mode         t) ;; Remember last cursor location in file

(add-to-list 'load-path "~/.config/emacs/opal")
(require 'opal-lsp-boost)
(require 'opal-hydra)
(require 'opal-evil)
(require 'opal-vc)
(require 'opal-syntax)
(require 'opal-theme)
(require 'opal-projectile)
(require 'opal-lsp)
(require 'opal-completion)
(require 'opal-help)
(require 'opal-workspaces)

(require 'opal-org)
(require 'opal-elisp)
(require 'opal-c)
(require 'opal-ocaml)
(require 'opal-nix)
(require 'opal-racket)
(require 'opal-python)

;; Re-enable garbage collection after startup
(setq
 gc-cons-threshold 100000000
 gc-cons-percentage 0.1)

(provide 'init)
;;; init.el ends here
