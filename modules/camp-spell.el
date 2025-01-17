;; -*- lexical-binding: t; -*-

;; Just-in-time spell checker
(use-package jinx
  :ensure t
  :config
  (+map!
    [remap ispell-word] #'jinx-correct)

  (dolist (hook '(text-mode-hook prog-mode-hook conf-mode-hook))
    (add-hook hook #'jinx-mode)))

(provide 'camp-spell)
