;; -*- lexical-binding: t; -*-

(use-package rg
  :straight t
  :defer t)

(use-package crux
  :straight t)

(use-package dockerfile-mode
  :straight t)

(use-package eat
  :straight t)

(defun eat-toggle ()
  (interactive)
  (if (string= (buffer-name) "*eat*")
      (delete-window)
    (eat-other-window "fish" nil)))

(provide 'camp-tools)
