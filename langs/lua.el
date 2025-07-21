;; -*- lexical-binding: t; -*-

(use-package lua-ts-mode
  :disabled
  :mode "\\.lua$")

(use-package lua-mode
  :ensure t
  :mode "\\.lua$"
  :interpreter "lua"
  :config
  (add-hook 'python-ts-mode-hook 'eglot-ensure))

(provide 'camp-lua)
