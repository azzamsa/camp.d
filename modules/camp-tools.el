;; -*- lexical-binding: t; -*-

(use-package rg
  :ensure t
  :defer t)

(use-package crux
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package eat
  :ensure t
  :config
  (keymap-global-set "C-/" 'eat-toggle))

(defun eat-toggle ()
  (interactive)
  (if (string= (buffer-name) "*eat*")
      (delete-window)
    (eat-other-window "fish" nil)))

(provide 'camp-tools)
