;;; opal-ocaml.el --- caMeLs -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'opal-package)

(require 'ocp-indent)
(use-package tuareg
  :mode ("\\.ml\\'" . tuareg-mode)
  :config
  (add-hook 'tuareg-mode
            (lambda ()
              (when (functionp 'prettify-symbols-mode)
                (prettify-symbols-mode)))))

(provide 'opal-ocaml)
;;; opal-ocaml.el ends here
