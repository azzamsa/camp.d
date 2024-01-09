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

(use-package consult-lsp :straight t)

(use-package lsp-mode
  :straight t
  :after camp-loaded
  :commands (lsp lsp-deferred)
  :hook ((before-save . lsp-format-buffer)
         (before-save . lsp-organize-imports))
  :hook ((rust-ts-mode . lsp-deferred)
         (web-mode . lsp-deferred))
  :config
  ;; Disable invasive lsp-mode features
  (setq lsp-ui-sideline-enable nil   ; not anymore useful than flycheck
        lsp-ui-doc-enable nil        ; slow and redundant with K
        lsp-enable-symbol-highlighting nil
        ;; If an LSP server isn't present when I start a prog-mode buffer, you
        ;; don't need to tell me. I know. On some systems I don't care to have a
        ;; whole development environment for some ecosystems.
        +lsp-prompt-to-install-server nil
        ;; shut down if all buffer killed
        lsp-keep-workspace-alive nil

        lsp-session-file (expand-file-name "lsp-session" camp-cache-dir)
        lsp-completion-provider :none))

(use-package lsp-ui
  :straight t
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-enable nil  ; no more useful than flycheck
        ;; redundant with K
        lsp-ui-doc-enable nil))

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

(use-package typescript-ts-mode
  :straight (:type built-in)
  :mode "\\.ts\\'")

(use-package web-mode
  :straight t
  :mode ("\\.njk\\'" "\\.svelte\\'" "\\.html\\'"
         "\\.vue\\'"))

(use-package emmet-mode
  :straight t
  :hook (web-mode . emmet-mode)
  :after web-mode)

(use-package apheleia
  :straight t)

(use-package lua-mode :straight t)
(use-package fish-mode :straight t)

(provide 'camp-prog)
