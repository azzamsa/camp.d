;; -*- lexical-binding: t; -*-

;; It is the 21st century, should I save file manually?
(use-package super-save
  :ensure t
  :config
  (setq super-save-auto-save-when-idle t)
  ;; disable built-in auto-save.
  ;; There is no point of keeping the backups.
  (setq auto-save-default nil)

  (add-to-list 'super-save-triggers 'magit-status)
  (add-to-list 'super-save-triggers 'consult-buffer)
  (add-to-list 'super-save-triggers 'consult-recent-file)

  (setq super-save-all-buffers t)
  ;; Save silently
  (setq super-save-silent t)

  ;; Cleanup is handled by ws-butler
  ;; Enable deleting trailing white spaces before saving
  ;; (setq super-save-delete-trailing-whitespace t)

  ;; Need to explicitly load the mode
  (super-save-mode +1))

;; Visual Undo
(use-package vundo
  :ensure t
  :defer t
  :init
  (+map! "ov" #'vundo)
  :config
  (setq vundo-compact-display t
        vundo-window-max-height 6
        vundo-glyph-alist
        '((selected-node   . ?●)
          (node            . ?○)
          (vertical-stem   . ?│)
          (branch          . ?├)
          (last-branch     . ?╰)
          (horizontal-stem . ?─))))

(use-package undo-fu
  :ensure t
  :config
  (with-eval-after-load 'evil
    (evil-set-undo-system 'undo-fu)))

(use-package undo-fu-session
  :ensure t
  :after undo-fu
  :config
  (setq undo-fu-session-compression 'zst
        undo-fu-session-directory (expand-file-name "undo-fu-session" camp-var-dir))
  (global-undo-fu-session-mode 1))

(use-package unicode-fonts
  :ensure t
  :config
  (defun +unicode-fonts-setup ()
    "Prefer the `:unicode-font-family' from `camp-fonts'."
    (when-let ((frame (selected-frame)))
      (when (display-multi-font-p frame)
        (with-selected-frame frame
          (when-let ((unicode-font-family (plist-get camp-fonts :unicode-font-family)))
            (dolist (unicode-block unicode-fonts-block-font-mapping)
              (push unicode-font-family (cadr unicode-block))))
          (unicode-fonts-setup))))))

(use-package ligature
  :ensure t
  :hook (prog-mode . ligature-mode)
  :when (and (>= emacs-major-version 28)
             (string-search "HARFBUZZ" system-configuration-features)
             (string-search "CAIRO" system-configuration-features))
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures
   'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                "\\\\" "://")))

;; highlight yanked line
(use-package evil-goggles
  :ensure t
  :init
  (evil-goggles-mode))

(use-package drag-stuff
  :ensure t
  :defer t
  :init
  (+nvmap!
    "<M-up>"   '(drag-stuff-up   :wk "Drag up")
    "<M-down>" '(drag-stuff-down :wk "Drag down")))

(use-package indent-bars
  :ensure t
  :hook ((prog-mode text-mode conf-mode) . indent-bars-mode)
  :config
  (setq indent-bars-prefer-character
        (or
         ;; Bitmaps are far slower on MacOS, inexplicably, but this needs more
         ;; testing to see if it's specific to ns or emacs-mac builds, or is
         ;; just a general MacOS issue.
         (featurep :system 'macos)
         ;; FIX: A bitmap init bug in emacs-pgtk (before v30) could cause
         ;; crashes (see jdtsmith/indent-bars#3).
         (and (featurep 'pgtk)
              (< emacs-major-version 30)))

        ;; Show indent guides starting from the first column.
        indent-bars-starting-column 0
        ;; Make indent guides subtle; the default is too distractingly colorful.
        indent-bars-width-frac 0.1  ; make bitmaps thinner
        indent-bars-color-by-depth nil
        indent-bars-color '(font-lock-comment-face :face-bg nil :blend 0.250)
        ;; Don't highlight current level indentation; it's distracting and is
        ;; unnecessary overhead for little benefit.
        indent-bars-highlight-current-depth nil))

(use-package expand-region
  :ensure t
  :init
  (+vmap! "v" #'er/expand-region))

(use-package helpful
  ;; a better *help* buffer
  :ensure t
  :commands helpful--read-symbol
  :hook (helpful-mode . visual-line-mode)
  :init
  ;; Make `apropos' et co search more extensively. They're more useful this way.
  (setq apropos-do-all t)

  (global-set-key [remap describe-function] #'helpful-callable)
  (global-set-key [remap describe-command]  #'helpful-command)
  (global-set-key [remap describe-variable] #'helpful-variable)
  (global-set-key [remap describe-key]      #'helpful-key)
  (global-set-key [remap describe-symbol]   #'helpful-symbol))

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

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;; Clean only edited lines
(use-package ws-butler
  :ensure t
  :config
  (ws-butler-global-mode t))

(use-package dogears
  :ensure t)

(provide 'cam-editor)
