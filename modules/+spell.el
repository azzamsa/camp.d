;; -*- lexical-binding: t; -*-

;; Just-in-time spell checker
(use-package jinx
  :ensure t
  :hook (emacs-startup . global-jinx-mode)
  :bind ([remap ispell-word] . jinx-correct)
  :config
  (setq jinx-languages "en_US id_ID"))

;; Distraction-free words correction with `flyspell' via `completing-read'
(use-package flyspell-correct
  :ensure t)

(provide '+spell)
