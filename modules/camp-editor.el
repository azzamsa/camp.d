;; -*- lexical-binding: t; -*-

;; It is the 21st century, should I save file manually?
(use-package super-save
  :straight t
  :defer 3
  :config
  (add-to-list 'super-save-triggers 'vertico)
  (add-to-list 'super-save-triggers 'magit)
  (add-to-list 'super-save-triggers 'find-file)
  (add-to-list 'super-save-triggers 'winner-undo)
  ;; Need to explicitly load the mode
  (super-save-mode +1))

;; Visual Undo
(use-package vundo
  :straight t
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
  :straight t
  :config
  (with-eval-after-load 'evil
    (evil-set-undo-system 'undo-fu)))

(use-package undo-fu-session
  :straight t
  :after undo-fu
  :config
  (setq undo-fu-session-compression 'zst
        undo-fu-session-directory (expand-file-name "undo-fu-session" camp-var-dir))
  (global-undo-fu-session-mode 1))

(use-package unicode-fonts
  :straight t
  :after camp-loaded
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
  :straight t
  :after camp-loaded
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
  :straight t
  :init
  (evil-goggles-mode))

(use-package drag-stuff
  :straight t
  :defer t
  :bind (("<M-up>"   . 'drag-stuff-up)
         ("<M-down>"  . 'drag-stuff-down)
         ("<M-left>"  . 'drag-stuff-left)
         ("<M-right>" . 'drag-stuff-right)))

(use-package highlight-indent-guides
  :straight t
  :hook ((prog-mode text-mode conf-mode) . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

(use-package expand-region
  :straight t
  :init
  (+vmap! "v" #'er/expand-region))

(use-package helpful
  ;; a better *help* buffer
  :straight t
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
  :straight t
  :defer t
  :commands (yas-minor-mode-on
             yas-expand
             yas-expand-snippet
             yas-lookup-snippet
             yas-insert-snippet
             yas-new-snippet
             yas-visit-snippet-file
             yas-activate-extra-mode
             yas-deactivate-extra-mode
             yas-maybe-expand-abbrev-key-filter)
  :init
  ;; Reduce default verbosity. 3 is too chatty about initializing yasnippet. 2
  ;; is just right (only shows errors).
  (defvar yas-verbosity 2)
  ;; Remove default ~/.emacs.d/snippets
  (defvar yas-snippet-dirs nil)
  :config
  ;; ~/.emacs.d/etc/yasnippet/snippets
  (setq private-yas-dir (expand-file-name "yasnippet/snippets/"camp-etc-dir))
  (+ensure-directory private-yas-dir)
  (push private-yas-dir yas-snippet-dirs)

  (yas-reload-all)
  (yas-global-mode +1))

(use-package yasnippet-snippets
  :straight t
  :after yasnippet)

(provide 'cam-editor)
