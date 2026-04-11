;; -*- lexical-binding: t; -*-

(use-package js-ts-mode
  :mode ("\\.js\\'" . js-ts-mode))

(use-package typescript-ts-mode
  :mode "\\.ts\\'")

(use-package web-mode
  :ensure t
  :mode ("\\.njk\\'" "\\.svelte\\'" "\\.html\\'"
         "\\.vue\\'"))

(use-package emmet-mode
  :ensure t
  :hook (web-mode . emmet-mode)
  :after web-mode)

(provide '+web)
