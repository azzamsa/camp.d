;; -*- lexical-binding: t; -*-

(use-package treesit-auto
  :ensure t
  :after treesit
  :custom
  (treesit-auto-install 'prompt)
  :config
  (add-to-list 'treesit-auto-recipe-list
               (make-treesit-auto-recipe
                :lang 'rust
                :ts-mode 'rust-ts-mode
                :remap 'rust-mode
                :url "https://github.com/tree-sitter/tree-sitter-rust"
                :revision "v0.23.3"
                :ext "\\.rs\\'"))
  (setq major-mode-remap-alist
        (treesit-auto--build-major-mode-remap-alist))

  (setq treesit-auto-langs '(rust))
  (treesit-auto-add-to-auto-mode-alist '(rust))
  (global-treesit-auto-mode))

(use-package editorconfig
  :ensure t)

(use-package rainbow-mode
  :ensure t
  :hook (prog-mode text-mode conf-mode))

(use-package colorful-mode
  :disabled
  :ensure t
  :hook (prog-mode text-mode))

;; Emacs rainbow delimiters mode
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-numbers
  :ensure t
  :hook (prog-mode . highlight-numbers-mode))

(use-package smartparens
  :ensure t
  :config
  (sp-local-pair 'markdown-mode "```" "```")
  (smartparens-global-mode))

;; Run code formatter on buffer contents without moving point
(use-package apheleia
  :ensure t
  :config
  ;; Don't assume Emacs indentation is correct
  (setq apheleia-formatters-respect-indent-level nil)
  ;; Format remote files using local formatters
  (setq apheleia-remote-algorithm 'local)

  ;; Add more formatters
  (dolist (mode '(python-mode python-ts-mode))
    (setf (alist-get mode apheleia-mode-alist) 'ruff))

  ;; Define the formatter
  (dolist
      (formatter-cmd '((just-fmt . ("just" "--fmt" "--unstable" "--justfile" filepath))))
    (add-to-list #'apheleia-formatters formatter-cmd))

  (setf (alist-get 'just-mode apheleia-mode-alist) '(just-fmt))

  (apheleia-global-mode +1))

;; Highlight TODO keywords
(use-package hl-todo
  :ensure (:host github :repo "tarsius/hl-todo")
  :hook (prog-mode . hl-todo-mode)
  :config
  (cl-callf append hl-todo-keyword-faces
    '(("BUG"   . "#ee5555")
      ("FIX"   . "#0fa050")
      ("PROJ"  . "#447f44")
      ("IDEA"  . "#0fa050")
      ("INFO"  . "#0e9030")
      ("TWEAK" . "#fe9030")
      ("PERF"  . "#e09030"))))

(provide 'camp-prog)
