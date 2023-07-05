;; -*- lexical-binding: t; -*-

(require 'cl-lib)

(setq inhibit-splash-screen nil)

(defvar camp-splash-buffer-name "*camp-splash*")

(defun camp-splash-screen ()
  "camp splash screen"
  (interactive)
  (let* ((splash-buffer  (get-buffer-create camp-splash-buffer-name))
         (recover-session (and auto-save-list-file-prefix
                               (file-directory-p (file-name-directory
                                                  auto-save-list-file-prefix))))
         (height         (- (window-body-height nil) 1))
         (width          (window-body-width nil))
         (padding-center (- (/ height 2) 1))
         (padding-bottom (- height (/ height 2) 3)))

    ;; If there are buffer associated with filenames,
    ;;  we don't show splash screen.
    (if (eq 0 (length (cl-loop for buf in (buffer-list)
                               if (buffer-file-name buf)
                               collect (buffer-file-name buf))))

        (with-current-buffer splash-buffer
          (erase-buffer)

          ;; Buffer local settings
          (if (one-window-p)
              (setq mode-line-format nil))
          (setq cursor-type nil
                vertical-scroll-bar nil
                horizontal-scroll-bar nil
                fill-column width)
          (face-remap-add-relative 'link :underline nil)

          ;; Vertical padding to center
          (insert-char ?\n padding-center)

          ;; Central text
          (insert (concat (propertize " 🏕️ Camp " 'face 'bold)
                          (format "(GNU Emacs %s) " emacs-version)))
          (center-line) (insert "\n")
          (insert (propertize (format "Loaded in %s" (emacs-init-time)) 'face 'shadow))
          (center-line) (insert "\n")

          ;; Copyright text
          (center-line) (insert "\n")
          (insert (propertize (format "Want to go camping?") 'face 'warnings))
          (center-line) (insert "\n")

          ;; Vertical padding to bottom
          (insert-char ?\n padding-bottom)

          (goto-char 0)
          (read-only-mode t)

          (local-set-key (kbd "C-[")       'camp-splash-screen-kill)
          (local-set-key (kbd "<escape>")  'camp-splash-screen-kill)
          (local-set-key (kbd "q")         'camp-splash-screen-kill)
          (local-set-key (kbd "<mouse-1>") 'mouse-set-point)
          (local-set-key (kbd "<mouse-2>") 'operate-this-button)
          (display-buffer-same-window splash-buffer nil)
          (when evil-mode
            (evil-local-mode -1))))))

(defun camp-splash-screen-kill ()
  "Kill the splash screen buffer (immediately)."
  (interactive)
  (when (get-buffer camp-splash-buffer-name)
    (kill-buffer camp-splash-buffer-name)))

;; Suppress any startup message in the echo area
(run-with-idle-timer 0.05 nil (lambda() (message nil)))

;; Install hook after frame parameters have been applied and only if
;; no option on the command line
(when (and (not inhibit-startup-screen)
           (not (member "-no-splash"  command-line-args))
           (not (member "--file"      command-line-args))
           (not (member "--insert"    command-line-args))
           (not (member "--find-file" command-line-args)))
  (add-hook 'window-setup-hook 'camp-splash-screen)
  (setq inhibit-startup-screen t
        inhibit-startup-message t
        inhibit-startup-echo-area-message t))

;; Close splash screen automatically after Emacs gets loaded
(add-hook 'emacs-startup-hook
          (defun +camp-splash--kill-h ()
            (run-at-time 2 nil #'camp-splash-screen-kill)))

(provide 'camp-splash)
