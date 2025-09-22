;; -*- lexical-binding: t; -*-

;; A template system for Emacs
(use-package yasnippet
  :ensure t
  :config
  (setq private-yas-dir (no-littering-expand-etc-file-name "yasnippet/snippets"))
  (push private-yas-dir yas-snippet-dirs)

  (yas-reload-all)
  (yas-global-mode +1))

;; A collection of yasnippet snippets for many languages
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;; The Doom Emacs snippets library
;; (use-package doom-snippets
;;   :ensure (:host github :repo "hlissner/doom-snippets" :files ("*.el" "*"))
;;   :after yasnippet)

(provide 'camp-snippets)
