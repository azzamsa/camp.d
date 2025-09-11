;; -*- lexical-binding: t; -*-

;; Keep ~/.emacs.d/ clean.
(use-package no-littering
  :ensure t
  :demand t
  :config
  (setq no-littering-etc-directory
        (expand-file-name "etc/" user-emacs-directory))
  (setq no-littering-var-directory
        (expand-file-name "var/" user-emacs-directory)))

(provide 'camp-early)
