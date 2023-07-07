;; -*- lexical-binding: t; -*-

(use-package transient
  :straight (:type built-in)
  :config
  ;; Map ESC and q to quit transient
  (keymap-set transient-map "<escape>" 'transient-quit-one)
  (keymap-set transient-map "q" 'transient-quit-one))

(use-package password-cache
  :straight (:type built-in)
  :custom
  (password-cache t) ; Enable password caching
  (password-cache-expiry 60)) ; One minute, default is 16

(use-package auth-source
  :straight (:type built-in)
  :custom
  (auth-sources '("~/.authinfo.gpg")) ; Default auth-sources to GPG
  (auth-source-do-cache t) ; Enable caching, do not keep asking about GPG key
  (auth-source-cache-expiry 86400)) ; All day, default is 2h (7200)

(use-package dired
  :straight (:type built-in))

(use-package tramp
  :straight (:type built-in)
  :init
  (setq tramp-default-method "ssh")
  :custom
  (tramp-default-remote-shell "/bin/bash"))

(use-package abbrev
  :straight (:type built-in)
  :init
  (setq-default abbrev-mode t)
  :config
  (setq save-abbrevs 'silently))

(use-package tab-bar
  :straight (:type built-in)
  :custom
  (tab-bar-show nil))

(use-package elisp-mode
  ;; defined in lisp/progmodes/elisp-mode.el
  ;; using `use-package emacs-lisp-mode' produces
  ;; so many oddities
  :straight (:type built-in)
  :after evil evil-collection camp-loaded
  :hook (prog-mode-defaults . emacs-lisp-mode)
  :config
  (evil-collection-define-key 'normal 'emacs-lisp-mode-map
    "gz" 'nil))

(use-package winner
  :straight (:type built-in)
  :config
  (winner-mode 1))

(provide 'camp-builtin)
