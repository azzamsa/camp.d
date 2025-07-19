;; -*- lexical-binding: t; -*-

;; A template system for Emacs
(use-package yasnippet
  :ensure t
  :init
  ;; Reduce default verbosity. 3 is too chatty about initializing yasnippet. 2
  ;; is just right (only shows errors).
  (defvar yas-verbosity 2)
  ;; Remove default ~/.emacs.d/snippets
  (defvar yas-snippet-dirs nil)
  :config
  ;; ~/.emacs.d/etc/yasnippet/snippets
  (setq private-yas-dir (expand-file-name "yasnippet/snippets/" camp-etc-dir))
  (+ensure-directory private-yas-dir)
  (push private-yas-dir yas-snippet-dirs)

  (yas-reload-all)
  (yas-global-mode +1))


;; A collection of yasnippet snippets for many languages
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;; The Doom Emacs snippets library
(use-package doom-snippets
  :ensure (:host github :repo "hlissner/doom-snippets" :files ("*.el" "*")))

(provide 'camp-snippets)
