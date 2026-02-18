;; -*- lexical-binding: t; -*-

;; Collection of useful dired additions
(use-package dired-hacks
  :ensure (:host github :repo "Fuco1/dired-hacks"))

(use-package dirvish
  :ensure t
  :after dired
  :init
  (setq dirvish-hide-details t)
  :config
  (dirvish-override-dired-mode)
  (setq dirvish-attributes '(nerd-icons subtree-state file-size vc-state git-msg))
  (setq dirvish-cache-dir (expand-file-name "dirvish" camp-cache-dir)))

(provide '+files)
