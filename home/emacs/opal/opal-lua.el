;;; opal-lua.el --- lua mode stuff -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'opal-package)

(use-package lua-ts-mode
  :mode "\\.lua$"
  :interpreter "lua")

;; NOTE(emi): Fallback just in case
(use-package lua-mode)

(provide 'opal-lua)
;;; opal-lua.el ends here
