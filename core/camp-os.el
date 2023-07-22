;; -*- lexical-binding: t; -*-

;; Without this package, several Emacs packages fail to locate the necessary binary files.
(use-package exec-path-from-shell
  :straight t
  :demand t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(provide 'camp-os)
