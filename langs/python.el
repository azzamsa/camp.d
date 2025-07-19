;; -*- lexical-binding: t; -*-

(use-package python-ts-mode
  :ensure nil
  :mode "\\.py\\'"
  :config
  (add-to-list 'eglot-server-programs
               '(python-ts-mode . ("pylsp")))
  (add-hook 'python-ts-mode-hook 'eglot-ensure))

(provide 'camp-python)
