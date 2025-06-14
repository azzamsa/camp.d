;; -*- lexical-binding: t; -*-

(use-package treesit-auto
  :disabled
  :ensure t
  :after treesit
  :custom
  (treesit-auto-install 'prompt)
  :config
  ;; Install all languages when calling `treesit-auto-install-all'
  (setq treesit-language-source-alist (treesit-auto--build-treesit-source-alist))
  ;; (treesit-auto-install-all)
  (global-treesit-auto-mode))

;; Extra non-standard functionalities for Eglot
(use-package eglot-x
	:after eglot
  :ensure (:host github :repo "nemethf/eglot-x")
  :commands (eglot-x-setup))

(use-package eglot-booster
  :ensure (:host github :repo "jdtsmith/eglot-booster")
	:after eglot
	:config	(eglot-booster-mode))

;; Consult integration with Eglot
(use-package consult-eglot
  :ensure t
  :config
  (consult-customize
   consult-eglot-symbols
   :initial (or (thing-at-point 'region t) (thing-at-point 'symbol t))))

(use-package lsp-mode
  :disabled
  :ensure t
  :commands (lsp lsp-deferred)
  :hook ((before-save . lsp-format-buffer)
         (before-save . lsp-organize-imports))
  :hook ((web-mode . lsp-deferred)
         (go-mode . lsp-deferred))
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
  :disabled
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-enable nil  ; no more useful than flycheck
        ;; redundant with K
        lsp-ui-doc-enable nil))

(use-package lsp-bridge
  :disabled
  :ensure '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
                       :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
                       :build (:not compile))
  :init
  (global-lsp-bridge-mode))

(use-package consult-lsp
  :disabled
  :ensure t)

(use-package editorconfig :ensure t)

(use-package rainbow-mode
  :ensure t
  :hook (prog-mode text-mode conf-mode))

(use-package colorful-mode
  :disabled
  :ensure t
  :hook (prog-mode text-mode))

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

(use-package rust-ts-mode
  :disabled
  :ensure nil
  :mode "\\.rs\\'")

(use-package rustic
  :ensure t
  :mode ("\\.rs$" . rustic-mode)
  :config
  (setq rustic-lsp-client 'eglot))

(use-package go-mode
  :ensure t
  :mode ("\\.go$" . go-mode))

(use-package typescript-ts-mode
  :ensure nil
  :mode "\\.ts\\'")

(use-package web-mode
  :ensure t
  :mode ("\\.njk\\'" "\\.svelte\\'" "\\.html\\'"
         "\\.vue\\'"))

(use-package emmet-mode
  :ensure t
  :hook (web-mode . emmet-mode)
  :after web-mode)

(use-package apheleia
  :ensure t
  :config
  (setq apheleia-formatters-respect-indent-level nil)
  (setq apheleia-remote-algorithm 'local) ; format remote files using local formatters
  (apheleia-global-mode +1))

(use-package lua-ts-mode
  :disabled
  :ensure nil)

(use-package lua-mode
  :ensure t
  :mode "\\.lua$"
  :interpreter "lua")

(use-package fish-mode :ensure t)

(provide 'camp-prog)
