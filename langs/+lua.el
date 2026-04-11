;; -*- lexical-binding: t; -*-

(use-package lua-ts-mode
  :mode "\\.lua$")

(use-package lua-mode
  :disabled
  :ensure t
  :mode "\\.lua$"
  :interpreter "lua")

(provide '+lua)
