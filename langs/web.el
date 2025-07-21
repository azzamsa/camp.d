;; -*- lexical-binding: t; -*-

(use-package js-ts-mode
  :mode ("\\.js\\'" . js-ts-mode)
  :config
  (add-hook 'js-ts-mode-hook 'eglot-ensure))

(use-package typescript-ts-mode
  :mode "\\.ts\\'"
  :config
  (add-hook 'typescript-ts-mode-hook 'eglot-ensure))

(use-package web-mode
  :ensure t
  :mode ("\\.njk\\'" "\\.svelte\\'" "\\.html\\'"
         "\\.vue\\'"))

(use-package emmet-mode
  :ensure t
  :hook (web-mode . emmet-mode)
  :after web-mode)

(provide 'camp-python)
