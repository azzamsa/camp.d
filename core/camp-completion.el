;; -*- lexical-binding: t; -*-

;;
;; In-Buffer completion

(use-package corfu
  ;; Enchanre built-in In-Buffer completion.
  ;; Company -> Corfu.
  :ensure t
  :config
  ;; Setup corfu for popup like completion
  (setq corfu-cycle t ; Allows cycling through candidates
        corfu-auto t ; Enable auto completion
        corfu-auto-prefix 2 ; Complete with less prefix keys
        corfu-auto-delay 0.0 ; No delay for completion
        corfu-min-width 25
        corfu-count 10
        corfu-scroll-margin 4
        corfu-preselect-first t
        corfu-echo-documentation 0.25) ; Echo docs for current completion option

  (defun +corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (corfu-mode 1)))

  (add-hook 'minibuffer-setup-hook #'+corfu-enable-in-minibuffer)
  (global-corfu-mode 1))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package cape
  ;; Completion at point extensions
  ;; Complete filename, dabbrev, emojis, etc.
  :ensure t
  :after corfu
  :config
  (add-to-list 'completion-at-point-functions #'cape-file) ;; complete file names
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(use-package kind-icon
  ;; Adds icons to In-Buffer completion.
  :ensure t
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
  (kind-icon-blend-background nil)  ; Use midpoint color between foreground and background colors ("blended")?
  (kind-icon-blend-frac 0.08)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)) ; Enable `kind-icon'

;;
;; Minibuffer completion.

(use-package nerd-icons-completion
  ;; Adds icons to Minifuffer completion.
  :ensure t
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)
  (nerd-icons-completion-mode))

(use-package embark
  ;; Actions during a Minibuffer completion session.
  :ensure t
  :bind
  ;; using `map!` doesn't work
  (("C-." . embark-act)
   ("C-," . embark-dwim)
   ("C-h B" . embark-bindings)
   (:map embark-file-map
         ("v" . +vsplit-file-open)))
  :config
  ;; Embark bug, the cursor doesn't follow the new buffer.
  (defun +vsplit-file-open (f)
    (+find-file-other-window-vertically f)))

(use-package embark-consult
  :ensure t)

(use-package marginalia
  ;; Informative minibuffer annotations
  :ensure t
  :config
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))

  ;; Icons integration
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)
  (marginalia-mode 1))

(use-package orderless
  ;; Filter `completion-at-point` and `completing-read` using regex, etc.
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  ;; Better `completing-read` in minibuffer.
  ;; Helm/Ivy/Ido -> Selectrum -> Vertico.
  :ensure t
  :config
  (setq vertico-cycle t
        vertico-count 12)

  (with-eval-after-load 'evil
    (define-key vertico-map (kbd "C-j") 'vertico-next)
    (define-key vertico-map (kbd "C-k") 'vertico-previous)
    (define-key vertico-map (kbd "M-h") 'vertico-directory-up)
    (define-key vertico-map '[C-left]  'vertico-previous-group)
    (define-key vertico-map '[C-right] 'vertico-next-group))

  (require 'vertico-directory)
  (vertico-mode 1))

(defun +vertico/project-search-from-cwd ()
  "Search files from the current working directory using Vertico and Consult."
  (interactive)
  (let ((dir (file-truename default-directory)))
    (consult-ripgrep dir)))

(use-package consult
  ;; Enhance `Vertico (completing-read)` with live-preview, grouping, narrowing/filtering, etc.
  :ensure t
  :custom
  ;; Use `consult-xref' for `xref-find-references'
  (xref-show-xrefs-function #'consult-xref)
  ;; Better formatting for `view-register'
  (register-preview-function #'consult-register-format)
  :init
  (keymap-set minibuffer-local-map "C-r"   'consult-history)
  (keymap-set minibuffer-local-map "C-S-v" 'consult-yank-pop)
  (keymap-global-set "C-s" 'consult-line)
  (+map!
    [remap bookmark-jump]                 '(consult-bookmark        :wk "Jump to bookmark")
    [remap evil-show-marks]               '(consult-mark            :wk "Show marks")
    [remap evil-show-registers]           '(consult-register        :wk "Show registers")
    [remap goto-line]                     '(consult-line            :wk "Go to line")
    [remap imenu]                         '(consult-imenu           :wk "Imenu")
    [remap Info-search]                   '(consult-info            :wk "Search in Info")
    [remap locate]                        '(consult-locate          :wk "Locate")
    [remap load-theme]                    '(consult-theme           :wk "Load theme")
    [remap man]                           '(consult-man             :wk "Man")
    [remap recentf-open-files]            '(consult-recent-file     :wk "Open recent file")
    [remap switch-to-buffer]              '(consult-buffer          :wk "Switch to buffer")
    [remap switch-to-buffer-other-window] '(consult-buffer-other-window :wk "Switch to buffer (other window)")
    [remap switch-to-buffer-other-frame]  '(consult-buffer-other-frame  :wk "Switch to buffer (other frame)")
    [remap yank-pop]                      '(consult-yank-pop        :wk "Yank pop")

     ;;; <leader> f --- file
    "fr"   '(consult-recent-file          :wk "Recent files"))
  :config
  (setq-default completion-in-region-function #'consult-completion-in-region)
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)
  ;; Hide all special buffers
  (setq consult-buffer-filter '("\\*.")))

(provide 'camp-completion)
