;; -*- lexical-binding: t; -*-

;;
;; Personal info
;;

(setq user-full-name "azzamsa"
      ;; prevent search engines from indexing the user's email address
      user-mail-address (concat "noreply" "@" "azzamsa" "." "com"))

;;
;; UI
;;

(setq +font-family "Maple Mono NF"
      +font-size 19
      +emoji-font "Twemoji")

(setq camp-fonts `(:font-family ,+font-family
                                :font-size ,+font-size
                                :variable-pitch-font-family ,+font-family
                                :variable-pitch-font-size ,+font-size
                                :unicode-font-family ,+emoji-font))

;;
;; Built-in
;;

(with-eval-after-load 'company
  (setq company-idle-delay 1))

(with-eval-after-load 'eldoc
  (setq eldoc-idle-delay 3))


;;
;; Custom Global Keybindings
;;

(keymap-global-set "C-s" (lambda ()
                           (interactive)
                           (basic-save-buffer)))
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
