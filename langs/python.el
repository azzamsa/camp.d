;; -*- lexical-binding: t; -*-

(use-package python-ts-mode
  :mode "\\.py\\'"
  :hook (python-ts-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '(python-ts-mode . ("pylsp"))))

(provide 'camp-python)
