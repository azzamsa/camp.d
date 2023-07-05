;; -*- lexical-binding: t; -*-

(use-package cape
  :straight t
  :after camp-loaded
  :config
  (add-to-list 'completion-at-point-functions #'cape-file) ;; complete file names
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(use-package corfu
  :straight t
  :after camp-loaded
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

(use-package kind-icon
  :straight t
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
  (kind-icon-blend-background nil)  ; Use midpoint color between foreground and background colors ("blended")?
  (kind-icon-blend-frac 0.08)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)) ; Enable `kind-icon'

(use-package embark
  :straight t
  :after camp-loaded
  :config
  (keymap-global-set "<remap> <describe-bindings>" #'embark-bindings)
  ;; Use Embark to show bindings in a key prefix with `C-h`
  (setq prefix-help-command #'embark-prefix-help-command)
  (+map! "a" #'embark-act))

(use-package embark-consult
  :straight t
  :after camp-loaded)

(use-package all-the-icons-completion
  :straight t
  :after camp-loaded)

(use-package marginalia
  :straight t
  :after camp-loaded
  :config
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))

  ;; Icons integration
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)
  (marginalia-mode 1))

(use-package orderless
  :straight t
  :after camp-loaded
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :straight t
  :after camp-loaded
  :config
  (add-to-list
   'load-path
   (expand-file-name
    (format "straight/%s/vertico/extensions" straight-build-dir)
    straight-base-dir))

  (setq vertico-cycle t
        vertico-count 12)

  (with-eval-after-load 'evil
    (define-key vertico-map (kbd "C-j") 'vertico-next)
    (define-key vertico-map (kbd "C-k") 'vertico-previous)
    (define-key vertico-map (kbd "M-h") 'vertico-directory-up))

  (require 'vertico-directory)
  (vertico-mode 1))

(use-package consult
  :straight t
  :after camp-loaded
  :custom
  ;; Use `consult-xref' for `xref-find-references'
  (xref-show-xrefs-function #'consult-xref)
  ;; Better formatting for `view-register'
  (register-preview-function #'consult-register-format)
  :init
  (keymap-set minibuffer-local-map "C-r"   'consult-history)
  (keymap-set minibuffer-local-map "C-S-v" 'consult-yank-pop)
  (keymap-global-set "C-s" 'consult-line)
  (keymap-global-set "C-s" 'consult-line)
  (+map!
    ;;; <leader> b --- buffer
    "bb"   '(consult-buffer               :wk "Search buffer")
    "bB"   '(consult-line-multi           :wk "Search all open buffers")
    "bl"   '(consult-line                 :wk "Search line")
    "bB"   '(consult-buffer-other-window  :wk "Switch buffer (other window)")
    "bF"   '(consult-buffer-other-frame   :wk "Switch buffer (other frame)")
    "bi"   '(consult-imenu                :wk "Imenu")
    "bO"   '(consult-outline              :wk "Outline")
     ;;; <leader> f --- file
    "fr"   '(consult-recent-file          :wk "Recent files")
    ;;; <leader> s --- search
    "ss"   '(consult-ripgrep              :wk "Ripgrep search")
    ;; project
    "pi"   '(consult-imenu-multi          :wk "Imenu multi")
    ;; insert
    "iy"   '(consult-yank-from-kill-ring  :wk "Yank from kill ring")
    "ir"   '(nil                          :wk "register")
    "irr"  '(consult-register             :wk "Consult register")
    "irl"  '(consult-register-load        :wk "Load register")
    "irs"  '(consult-register-store       :wk "Store register"))
  :config
  (setq-default completion-in-region-function #'consult-completion-in-region)
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

(provide 'camp-completion)
