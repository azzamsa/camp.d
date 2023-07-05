;; -*- lexical-binding: t; -*-

(defvar camp-lisp-modules
  '(camp aza files ui))

(defvar camp-core-modules
  '(defaults bootstrap builtin keybindings evil completion splash))

(defvar camp-modules
  '(data editor lisp prog project spell tools ui vc workspaces multi-cursors files))

(defun camp-reload (&optional without-core)
  "Reload all configuration, including user's config.el."
  (interactive)
  ;; Lisp modeles
  (dolist (module (mapcar #'symbol-name camp-lisp-modules))
    (+log "Loading lisp module \"%s\"" module)
    (load (expand-file-name (format "lisp/+%s.el" module) user-emacs-directory) nil (not init-file-debug)))

  ;; Core modules
  (dolist (module (mapcar #'symbol-name camp-core-modules))
    (+log "Loading core module \"%s\"" module)
    (load (expand-file-name (format "core/camp-%s.el" module) user-emacs-directory) nil (not init-file-debug)))

  ;; Modules
  (dolist (module (mapcar #'symbol-name camp-modules))
    (+log "Loading module \"%s\"" module)
    (load (expand-file-name (format "modules/camp-%s.el" module) user-emacs-directory) nil (not init-file-debug)))

  ;; Load user config when available
  (let ((user-config (expand-file-name "config.el" user-emacs-directory)))
    (when (file-exists-p user-config)
      (+log "Loading user config file from \"%s\"" user-config)
      (load user-config nil (not init-file-debug)))))

;; Load for the first time
(camp-reload)

;; Load fonts early (they are read from the default `camp-default-fonts').
(+set-fonts)

(+log "Loaded early-config.el")
