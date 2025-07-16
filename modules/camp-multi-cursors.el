;;; -*- lexical-binding: t; -*-

(use-package evil-multiedit
  :ensure t
  :after evil
  :demand t
  :init
  (+nvmap!
    "R"      '(evil-multiedit-match-all              :wk "Match all occurrences")
    "M-d"    '(evil-multiedit-match-symbol-and-next  :wk "Match symbol and go to next")
    "M-D"    '(evil-multiedit-match-symbol-and-prev  :wk "Match symbol and go to previous")
    "M-d"    '(evil-multiedit-match-and-next         :wk "Match and go to next")
    "M-D"    '(evil-multiedit-match-and-prev         :wk "Match and go to previous")
    "C-M-d"  '(evil-multiedit-restore                :wk "Restore previous match"))
  :config
  (evil-multiedit-default-keybinds))


(provide 'multi-cursors)
