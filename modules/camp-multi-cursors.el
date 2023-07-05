;;; -*- lexical-binding: t; -*-

(use-package evil-multiedit
  :straight t
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

(use-package evil-mc
  :straight t
  :after evil
  :init
  (+nvmap!
    "gzd"  '(evil-mc-make-and-goto-next-match       :wk "Make and goto next match")
    "gzD"  '(evil-mc-make-and-goto-prev-match       :wk "Make and goto previous match")
    "gzs"  '(evil-mc-skip-and-goto-next-match       :wk "Skip and goto next match")
    "gzS"  '(evil-mc-skip-and-goto-prev-match       :wk "Skip and goto previous match")
    "gzc"  '(evil-mc-skip-and-goto-next-cursor      :wk "Skip and goto next cursor")
    "gzC"  '(evil-mc-skip-and-goto-prev-cursor      :wk "Skip and goto previous cursor")
    "gzj"  '(evil-mc-make-cursor-move-next-line     :wk "Make cursor move to next line")
    "gzk"  '(evil-mc-make-cursor-move-prev-line     :wk "Make cursor move to previous line")
    "gzm"  '(evil-mc-make-all-cursors               :wk "Make all cursors")
    "gznn" '(evil-mc-make-and-goto-next-cursor      :wk "Make and goto next cursor")
    "gzNN" '(evil-mc-make-and-goto-last-cursor      :wk "Make and goto last cursor")
    "gznp" '(evil-mc-make-and-goto-prev-cursor      :wk "Make and goto previous cursor")
    "gzNP" '(evil-mc-make-and-goto-first-cursor     :wk "Make and goto first cursor")
    "gzq"  '(evil-mc-undo-all-cursors               :wk "Undo all cursors")
    "gzI"  '(evil-mc-make-cursor-in-visual-selection-beg :wk "Make cursor in visual selection begin")
    "gzA"  '(evil-mc-make-cursor-in-visual-selection-end :wk "Make cursor in visual selection end")))

(provide 'multi-cursors)
