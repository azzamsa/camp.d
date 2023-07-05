;; -*- lexical-binding: t; -*-

;;; Virtual module loaded at end of init.el (after custom.el)
;;; Used to synchronize loading some other stuff after loading Emacs

;; Maybe useful
(setq camp-loaded t)

(+log "Providing `camp-loaded'.")

(provide 'camp-loaded)
