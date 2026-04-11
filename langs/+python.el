;; -*- lexical-binding: t; -*-

(use-package python-ts-mode
  :mode "\\.py\\'"
  :config
  (add-to-list 'eglot-server-programs
               '(python-ts-mode . ("pylsp"))))

(provide '+python)
