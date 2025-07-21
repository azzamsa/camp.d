(use-package rust-ts-mode
  :disabled
  :mode "\\.rs\\'")

(use-package rustic
  :ensure t
  :mode ("\\.rs$" . rustic-mode)
  :config
  (setq rustic-lsp-client 'eglot)
  (add-hook 'rustic-mode-hook 'eglot-ensure))

(provide 'camp-rust)
