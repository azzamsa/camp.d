;; -*- lexical-binding: t; -*-

(use-package yaml-mode
  :ensure t
  :mode "Procfile\\'")

(use-package toml-mode
  :ensure t
  :mode "\\.toml\\'")

(use-package json-mode
  :ensure t
  :mode "\\.js\\(?:on\\|[hl]int\\(?:rc\\)?\\)\\'"
  :config
  (+map-local! :keymaps 'json-mode-map
    "p" #'json-mode-show-path
    "t" #'json-toggle-boolean
    "d" #'json-mode-kill-path
    "x" #'json-nullify-sexp
    "+" #'json-increment-number-at-point
    "-" #'json-decrement-number-at-point
    "f" #'json-mode-beautify))

(use-package just-mode
  :ensure t
  :defer t)

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode))

(use-package graphql-mode
  :ensure t
  :defer t)

(use-package vimrc-mode
  :ensure t
  :defer t)

(use-package nushell-mode
  :ensure t
  :defer t)

(use-package ledger-mode
  :ensure t
  :defer t
  :config
  (setq ledger-highlight-xact-under-point nil)
  (setq ledger-default-date-format ledger-iso-date-format))

(use-package flycheck-hledger
  :after (flycheck ledger-mode)
  :demand t)

(use-package beancount-mode
  :mode "\\.beancount\\'"
  :ensure (beancount-mode :type git :host github :repo "beancount/beancount-mode"
                          :files ("beancount.el")
                          :main "beancount.el"))

(use-package typst-ts-mode
  :ensure (:host codeberg :repo "meow_king/typst-ts-mode"))

(use-package scad-mode
  :ensure t)

(use-package ron-mode
  :ensure t
  :defer t
  :mode "\\.ron\\'")

(use-package pest-mode
  :ensure t
  :defer t)

(use-package hurl-mode
  :ensure (hurl-mode :type git :host github :repo "jaszhe/hurl-mode"))

(provide 'camp-data)
