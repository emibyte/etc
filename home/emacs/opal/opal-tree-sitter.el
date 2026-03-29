;;; opal-tree-sitter.el --- tree-sitter-modes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq major-mode-remap-alist
      '(
        ;; (yaml-mode . yaml-ts-mode)
        (sh-mode . bash-ts-mode)
        (c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)
        (css-mode . css-ts-mode)
        (python-mode . python-ts-mode)
        (mhtml-mode . html-ts-mode)
        (javascript-mode . js-ts-mode)
        (js-json-mode . json-ts-mode)
        ;; (typescript-mode . typescript-ts-mode)
        (conf-toml-mode . toml-ts-mode)
        ;; (go-mode . go-mode)
        ;; (gdscript-mode . gdscript-ts-mode)
        ))
(setq treesit-font-lock-level 3)

(use-package cmake-ts-mode :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))
(use-package go-mod-ts-mode :mode "\\.mod\\'")
(use-package rust-ts-mode :mode "\\.rs\\'")
(use-package lua-ts-mode :mode "\\.lua\\'")
(use-package typescript-ts-mode :mode "\\.ts\\'")
(use-package tsx-ts-mode :mode "\\.tsx\\'")
(use-package yaml-ts-mode :mode ("\\.yaml\\'" "\\.yml\\'"))

(provide 'opal-tree-sitter)
;;; opal-tree-sitter.el ends here
