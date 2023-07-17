;; -*- lexical-binding: t; -*-

(use-package treesit-auto
  :straight (:host github :repo "renzmann/treesit-auto")
  :after treesit camp-loaded
  :custom
  (treesit-auto-install 'prompt)
  :config
  ;; Install all languages when calling `treesit-auto-install-all'
  (setq treesit-language-source-alist (treesit-auto--build-treesit-source-alist))
  ;; (treesit-auto-install-all)
  (global-treesit-auto-mode))

;;; Eglot + LSP
(use-package eglot
  :straight t
  :init
  (+map! :keymaps 'eglot-mode-map
    :infix "c"
    "fF" #'eglot-format-buffer
    "d"  '(eglot-find-declaration :wk "Find declaration")
    "i"  '(eglot-find-implementation :wk "Find implementation")
    "t"  '(eglot-find-typeDefinition :wk "Find type definition")
    "a"  '(eglot-code-actions :wk "Code actions")
    "r"  '(nil :wk "refactor")
    "rr" '(eglot-rename :wk "Rename")
    "rR" '(eglot-code-action-rewrite :wk "Rewrite")
    "rf" '(eglot-code-action-quickfix :wk "Quick fix")
    "ri" '(eglot-code-action-inline :wk "Inline")
    "re" '(eglot-code-action-extract :wk "Extract")
    "ro" '(eglot-code-action-organize-imports :wk "Organize imports")
    "eq" '(eglot-shutdown :wk "Shutdown")
    "er" '(eglot-reconnect :wk "Reconnect")
    "eQ" '(eglot-shutdown-all :wk "Shutdown all")
    "w"  '(eglot-show-workspace-configuration :wk "Eglot workspace config"))
  :config
  (defun +eglot-auto-enable ()
    "Auto-enable Eglot in configured modes in `+eglot-auto-enable-modes'."
    (interactive)
    (dolist (mode +eglot-auto-enable-modes)
      (let ((hook (intern (format "%s-hook" mode))))
        (add-hook hook #'eglot-ensure))))

  ;; Modified from Crafted Emacs, pass `eglot-server-programs' to this function
  ;; to fill `+eglot-auto-enable-modes' with all supported modes.
  (defun +eglot-use-on-all-supported-modes (mode-list)
    (dolist (mode-def mode-list)
      (let ((mode (if (listp mode-def) (car mode-def) mode-def)))
        (cond
         ((listp mode) (+eglot-use-on-all-supported-modes mode))
         (t
          (when (and (not (eq 'clojure-mode mode)) ; prefer cider
                     (not (eq 'lisp-mode mode))    ; prefer sly
                     (not (eq 'scheme-mode mode))) ; prefer geiser
            (add-to-list '+eglot-auto-enable-modes mode)))))))
  )

(use-package editorconfig :straight t)

(use-package rainbow-mode
  :straight t
  :hook (prog-mode . rainbow-mode))

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-numbers
  :straight t
  :hook (prog-mode . highlight-numbers-mode))

(use-package smartparens
  :straight t
  :config
  (sp-local-pair 'markdown-mode "```" "```")
  (smartparens-global-mode))

(use-package flymake :straight t)

(use-package rust-ts-mode
  :straight (:type built-in)
  :mode "\\.rs\\'")

(use-package web-mode
  :straight t
  :defer t
  :mode "\\.njk\\'")

(use-package lua-mode :straight t)
(use-package fish-mode :straight t)

(provide 'camp-prog)
