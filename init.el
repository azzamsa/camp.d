;; -*- lexical-binding: t; -*-

(defvar camp-core-modules
  '(bootstrap builtin keybindings evil completion os utils vars))

(defvar camp-modules
  '(snacks data editor prog project spell tools ui theme vc workspaces multi-cursors files))

(defun camp-reload (&optional without-core)
  "Reload all configuration, including user's config.el."
  (interactive)
  ;; Core
  (dolist (module (mapcar #'symbol-name camp-core-modules))
    (+log "Loading core module \"%s\"" module)
    (load (expand-file-name (format "core/camp-%s.el" module) user-emacs-directory) nil (not init-file-debug)))

  ;; Modules
  (dolist (module (mapcar #'symbol-name camp-modules))
    (+log "Loading module \"%s\"" module)
    (load (expand-file-name (format "modules/camp-%s.el" module) user-emacs-directory) nil (not init-file-debug)))

  ;; Load user config when available
  (let ((config (expand-file-name "config.el" user-emacs-directory)))
    (when (file-exists-p config)
      (+log "Loading user config file from \"%s\"" config)
      (load config nil (not init-file-debug))))

  ;; Load personal config when available
  (let ((config (expand-file-name "personal.el" user-emacs-directory)))
    (when (file-exists-p config)
      (+log "Loading personal config file from \"%s\"" config)
      (load config nil (not init-file-debug)))))

;; Load for the first time
(camp-reload)

;; Load fonts early (they are read from the default `camp-default-fonts').
(+set-fonts)

(+log "Loaded init.el")

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
