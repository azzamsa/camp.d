;; -*- lexical-binding: t; -*-

(use-package rg
  :ensure t
  :defer t)

;; Collection of Ridiculously Useful eXtensions for Emacs
(use-package crux
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

;; Emulate A Terminal, in a region, in a buffer and in Eshell
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
