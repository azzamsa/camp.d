;; -*- lexical-binding: t; -*-

(use-package lua-ts-mode
  :disabled
  :ensure nil)

(use-package lua-mode
  :ensure t
  :mode "\\.lua$"
  :interpreter "lua"
  :config
  (add-hook 'python-ts-mode-hook 'eglot-ensure))

(provide 'camp-lua)
