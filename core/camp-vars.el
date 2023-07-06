;; -*- lexical-binding: t; -*-

;; The directory for package configuration files
(defvar camp-etc-dir (expand-file-name "etc/" user-emacs-directory))
;; The directory for package data
(defvar camp-var-dir (expand-file-name "var/" user-emacs-directory))
(defvar camp-cache-dir (expand-file-name "cache/" camp-var-dir))

(defvar camp-after-startup nil
  "This hook will be run after loading Emacs.")

;; This will be set by the virtual package `camp-loaded'
(defvar camp-loaded nil
  "camp has been loaded.")

(defcustom camp-leader-key "SPC"
  "camp leader key."
  :group 'camp-keybinding
  :type 'string)

(defcustom camp-localleader-key "SPC m"
  "camp local leader (a.k.a. mode specific) key sequence."
  :group 'camp-keybinding
  :type 'string)

(defcustom camp-global-leader-prefix "C-SPC"
  "camp general leader key."
  :group 'camp-keybinding
  :type 'string)

(defcustom camp-global-mode-prefix "C-SPC m"
  "camp general local leader (a.k.a. mode specific) key sequence."
  :group 'camp-keybinding
  :type 'string)

(defcustom camp-theme 'doom-one
  "The theme of camp."
  :group 'camp-ui
  :type 'symbol)

(defcustom camp-fonts nil
  "Fonts to use within camp."
  :group 'camp-ui
  :type '(plist
          (:font-family string)
          (:font-size natnum)
          (:unicode-font-family string)
          (:variable-pitch-font-family string)
          (:variable-pitch-font-size natnum)))

(defcustom camp-after-set-fonts-hook nil
  "Runs after setting camp fonts, runs at the end of `+set-fonts'."
  :group 'camp-ui
  :type 'hook)

;; Setup default fonts
(let ((mono-font "monospace")
      (varp-font "monospace"))
  (defconst camp-default-fonts
    `(:font-family ,mono-font
                   :font-size 18
                   :unicode-font-family nil
                   :variable-pitch-font-family ,varp-font
                   :variable-pitch-font-size 18)
    "Default fonts of camp."))

(defcustom +eglot-auto-enable-modes
  '(python-mode python-ts-mode rust-mode rust-ts-mode
                js-mode js-ts-mode typescript-mode typescript-ts-mode
                json-mode json-ts-mode js-json-mode)
  "Modes for which Eglot can be automatically enabled by `+eglot-auto-enable'."
  :group 'camp-prog
  :type '(repeat symbol))

(defvar camp-scratch-file nil
  "Location of Camp persistence scratch buffer.")

(defvar camp-abbrev-file nil
  "Path to the abbreviations file.")

(provide 'camp-vars)
