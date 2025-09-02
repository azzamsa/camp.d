;; -*- lexical-binding: t; -*-

(use-package lua-ts-mode
  :mode "\\.lua$")

(use-package lua-mode
  :disabled
  :ensure t
  :mode "\\.lua$"
  :interpreter "lua"
  :config
  (add-hook 'lua-mode-hook 'eglot-ensure))

(provide 'camp-lua)
