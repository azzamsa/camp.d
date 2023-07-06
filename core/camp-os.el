;; -*- lexical-binding: t; -*-

;; Without this package lsp-mode or eglot-mode can't find
;; the servers
(use-package exec-path-from-shell
  :straight t
  :demand t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(provide 'camp-os)
