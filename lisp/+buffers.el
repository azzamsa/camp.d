;;; -*- lexical-binding: t; -*-

;;
;; Functions
;;

;;;###autoload
(defun doom-fallback-buffer ()
  "Returns the fallback buffer, creating it if necessary. By default this is the
scratch buffer. See `doom-fallback-buffer-name' to change this."
  (let (buffer-list-update-hook)
    (get-buffer-create doom-fallback-buffer-name)))

;;;###autoload
(defalias 'doom-buffer-list #'buffer-list)

;;;###autoload
(defun doom-project-buffer-list (&optional project)
  "Return a list of buffers belonging to the specified PROJECT.

If PROJECT is nil, default to the current project.

If no project is active, return all buffers."
  (let ((buffers (doom-buffer-list)))
    (if-let* ((project-root
               (if project (expand-file-name project)
                 (doom-project-root))))
        (cl-loop for buf in buffers
                 if (projectile-project-buffer-p buf project-root)
                 collect buf)
      buffers)))

;;;###autoload
(defun doom-kill-buffer-and-windows (buffer)
  "Kill the buffer and delete all the windows it's displayed in."
  (dolist (window (get-buffer-window-list buffer))
    (unless (one-window-p t)
      (delete-window window)))
  (kill-buffer buffer))

;;
;; Interactive commands
;;

(defun doom--message-or-count (interactive message count)
  (if interactive
      (message message count)
    count))

;;;###autoload
(defun doom/kill-all-buffers (&optional buffer-list interactive)
  "Kill all buffers and closes their windows.

If the prefix arg is passed, doesn't close windows and only kill buffers that
belong to the current project."
  (interactive
   (list (if current-prefix-arg
             (doom-project-buffer-list)
           (doom-buffer-list))
         t))
  (if (null buffer-list)
      (message "No buffers to kill")
    (save-some-buffers)
    (delete-other-windows)
    (when (memq (current-buffer) buffer-list)
      (switch-to-buffer (doom-fallback-buffer)))
    (mapc #'kill-buffer buffer-list)
    (doom--message-or-count
     interactive "Killed %d buffers"
     (- (length buffer-list)
        (length (cl-remove-if-not #'buffer-live-p buffer-list))))))
