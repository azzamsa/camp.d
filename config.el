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

(setq camp-fonts '(:font-family "JetBrainsMono Nerd Font"
                                    :font-size 18
                                    :variable-pitch-font-family "JetBrainsMono Nerd Font"
                                    :variable-pitch-font-size 18
                                    :unicode-font-family "Twemoji"))

;;
;; Built-in
;;

(with-eval-after-load 'company
  (setq company-idle-delay 1))

(with-eval-after-load 'eldoc
  (setq eldoc-idle-delay 3))


;;
;; Camp
;;

(setq camp-scratch-file "~/.local/share/meta/**scratch**.md")
(+ensure-directory camp-scratch-file)

(setq camp-abbrev-file "~/.local/share/meta/abbrevs.el")
(setq abbrev-file-name (+ensure-directory camp-abbrev-file))

;;
;; Modules
;;

;; Emacs doesn't play well with fish
(setq shell-file-name "/bin/bash")

;; Module: `natural-langs' -- Package: `spell-fu'
(with-eval-after-load 'spell-fu
  ;; We can use Camp' helper macro `+spell-fu-register-dictionaries!'
  ;; to enable multi-language spell checking.
  (+spell-fu-register-dictionaries! "en" "id"))

(use-package ron-mode
  :straight t
  :defer t
  :mode "\\.ron\\'")

(use-package pest-mode :straight t :defer t)

(use-package hurl-mode
  :straight (hurl-mode
             :pin "b5e7256"
             :host github :repo "Orange-OpenSource/hurl" :files ("contrib/emacs/*.el")))
