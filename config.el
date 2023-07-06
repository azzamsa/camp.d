;; -*- lexical-binding: t; -*-

;;
;; Personal info
;;

(setq user-full-name "azzamsa"
      ;; prevent search engines from indexing the user's email address
      user-mail-address (concat "vcs" "@" "azzamsa" "." "com"))

;;
;; UI
;;

;;
;; Why <current theme name>?
;; 1.  `catpuccin-*' has so manny issue I need to fix myself
;; - [active region almost unnoticable · Issue #112 · catppuccin/emacs](https://github.com/catppuccin/emacs/issues/112)
;; - [No iedit faces specified (used by evil-multiedit) · Issue #108 · catppuccin/emacs](https://github.com/catppuccin/emacs/issues/108)
;; doesn't have noticeable region color during evil multi-cursor.
;; 2. `doom-palenight' too muted for me.
;; 2. `doom-material' has she same issue as catppuccin. The highlighted text is unnoticable.
(setq camp-theme 'doom-dracula)
;; Why <current font name>?
;; - `VictorMono Nerd Font' is too thin
(setq camp-fonts '(:font-family "Iosevka Nerd Font"
                                    :font-size 18
                                    :variable-pitch-font-family "Iosevka Nerd Font"
                                    :variable-pitch-font-size 18
                                    :unicode-font-family "Twemoji"))

;;
;; Buit-in
;;

(with-eval-after-load 'company
  (setq company-idle-delay 1))

(with-eval-after-load 'eldoc
  (setq eldoc-idle-delay 3))


;;
;; Camp
;;

(setq camp-scratch-file "~/.local/share/**scratch**.md")
(+ensure-directory camp-scratch-file)

(setq camp-abbrev-file "~/.local/share/meta/abbrevs.el")
(setq abbrev-file-name (+ensure-directory camp-abbrev-file))

;;
;; Modules
;;

;; Emacs doesn't play well with fish
(setq shell-file-name "/bin/zsh")

(with-eval-after-load 'eglot
  ;; Auto enable Eglot in modes `+eglot-auto-enable-modes' using
  ;; `+eglot-auto-enable' (from the `prog' module). You can use
  (+eglot-auto-enable)

  ;; You can use this to fill `+eglot-auto-enable-modes' with all supported
  ;; modes from `eglot-server-programs'
  (+eglot-use-on-all-supported-modes eglot-server-programs))

;; Module: `natural-langs' -- Package: `spell-fu'
(with-eval-after-load 'spell-fu
  ;; We can use Camp' helper macro `+spell-fu-register-dictionaries!'
  ;; to enable multi-language spell checking.
  (+spell-fu-register-dictionaries! "en" "id"))

(use-package lsp-tailwindcss
  :straight t
  :after web-mode
  :config
  ;; LSP-mode doesn't know what is njk producing `Unable to calculate the languageId for buffer …'
  (add-to-list 'lsp-language-id-configuration '(".*\\.njk$" . "html")))

(use-package ron-mode
  :straight t
  :defer t
  :mode "\\.ron\\'")

(use-package pest-mode :straight t :defer t)

(use-package hurl-mode
  :straight (hurl-mode
             :pin "b5e7256"
             :host github :repo "Orange-OpenSource/hurl" :files ("contrib/emacs/*.el")))
