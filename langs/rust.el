(use-package rust-ts-mode
  :mode "\\.rs$"
  :hook (rust-ts-mode . eglot-ensure)
  :config
  (add-hook 'before-save-hook #'eglot-format-buffer t t))

(use-package rustic
  :disabled
  :ensure t
  :mode ("\\.rs$" . rustic-mode)
  :config
  (setq rustic-lsp-client 'eglot)
  (add-hook 'rustic-mode-hook 'eglot-ensure))

(provide 'camp-rust)
