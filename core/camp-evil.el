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
  :init
  (+nvmap!
    "S" #'evil-surround-region
    "s" #'evil-surround-edit)
  :config
  (global-evil-surround-mode 1))

(use-package evil-nerd-commenter
  :straight t
  :after evil camp-loaded
  :init
  (+nvmap!
    "gc" #'evilnc-comment-operator
    "gC" #'evilnc-copy-and-comment-operator))

(provide 'camp-evil)
