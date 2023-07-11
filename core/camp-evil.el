;; -*- lexical-binding: t; -*-

(use-package evil
  :straight t
  :preface
  ;; Needed by `evil-collection'
  (setq evil-want-keybinding nil
        evil-want-integration t)
  :custom
  (evil-want-C-i-jump nil)
  (evil-want-fine-undo t)
  (evil-want-Y-yank-to-eol t)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-kill-on-visual-paste nil)
  ;; BUG: setting this to t triggers errors on pressing . to repeat command
  (evil-respect-visual-line-mode nil)
  (evil-ex-interactive-search-highlight 'selected-window)
  :config
  (+nvmap!
    "C-u" '(evil-scroll-up :wk "Scroll up"))

  (+map!
    ;; buffer
    "bN" '(evil-buffer-new :wk "New buffer")
    ;; window
    "ww" '(evil-window-next :wk "Next")
    "wW" '(evil-window-prev :wk "Previous")
    "ws" '(evil-window-split :wk "Split")
    "wv" '(evil-window-vsplit :wk "Vertical split")
    "wr" '(evil-window-rotate-downwards :wk "Rotate downwards")
    "wR" '(evil-window-rotate-upwards :wk "Rotate upwards")
    "w+" '(evil-window-increase-width :wk "Increase width")
    "w-" '(evil-window-decrease-width :wk "Decrease width"))
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :straight t
  :config
  (evil-collection-init))

(use-package evil-colemak-basics
  :straight t
  :init
  (setq evil-colemak-basics-layout-mod `mod-dh)
  (global-evil-colemak-basics-mode))

(use-package evil-snipe
  :straight t
  :after evil camp-loaded
  :custom
  (evil-snipe-scope 'buffer)
  (evil-snipe-smart-case t)
  (evil-snipe-auto-scroll t))

(use-package evil-surround
  :straight t
  :commands (global-evil-surround-mode
             evil-surround-edit
             evil-Surround-edit
             evil-surround-region)
  :init
  (+vmap!
    "S" '(evil-surround-region :wk "Surround region"))
  (+omap!
    "s" '(evil-surround-edit :wk "Surround edit")
    "S" '(evil-Surround-edit :wk "Surround edit"))
  :config
  (global-evil-surround-mode 1))

(use-package evil-snipe
  :straight t
  :commands evil-snipe-local-mode evil-snipe-override-local-mode
  :init
  (setq evil-snipe-smart-case t
        evil-snipe-scope 'line
        evil-snipe-repeat-scope 'visible
        evil-snipe-char-fold t)
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))

(use-package evil-nerd-commenter
  :straight t
  :after evil camp-loaded
  :init
  (+nvmap!
    "gc" '(evilnc-comment-operator :wk "Comment")
    "gC" '(evilnc-copy-and-comment-operator :wk "Copy and comment")))

(provide 'camp-evil)
