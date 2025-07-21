;; -*- lexical-binding: t; -*-

(use-package go-mode
  :disabled
  :ensure t
  :mode ("\\.go$" . go-mode))

(use-package go-ts-mode
  :mode "\\.go\\'"
  :hook (go-ts-mode . eglot-ensure)
  :config
  ;; default: 8
  (setq go-ts-mode-indent-offset 2)
  (add-hook 'before-save-hook #'eglot-format-buffer t t))

(use-package flycheck-golangci-lint
  :ensure t
  :hook (go-ts-mode . flycheck-golangci-lint-setup))

(provide 'camp-go)
