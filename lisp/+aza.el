;; -*- lexical-binding: t; -*-

;;;###autoload
(defun today ()
  "Inserts the current date."
  (interactive)
  (insert (format-time-string "%A, %B %e, %Y")))

;;;###autoload
(defun now ()
  "Inserts the current date and time."
  (interactive)
  (insert (format-time-string "%F %H:%M")))

(defun save-all-buffers-silently ()
  (save-some-buffers t))

;;;###autoload
(defun aza-kill-other-buffers ()
  "Kill all buffers but current buffer and special buffers.
(Buffer that start with '*' and white space ignored)"
  (interactive)
  (when (y-or-n-p "Save and kill all other buffers ? ")
    (save-all-buffers-silently)
    (let ((killed-bufs 0))
      (dolist (buffer (delq (current-buffer) (buffer-list)))
        (let ((name (buffer-name buffer)))
          (when (and name (not (string-equal name ""))
                     (/= (aref name 0) ?\s)
                     (string-match "^[^\*]" name))
            (cl-incf killed-bufs)
            (funcall 'kill-buffer buffer))))
      (message "Saved & killed %d buffer(s)" killed-bufs))))

;;;###autoload
(defun file-manager-here ()
  "Open current directory with default file manager."
  (interactive)
  (message "Opening file manager in current directory...")
  ;; `xdg-open' will pick the default file manager
  (start-process "" nil "xdg-open" "."))

;;;###autoload
(defun terminal-here ()
  "Open a new terminal with the current directory as PWD."
  (interactive)
  (message "Opening terminal in %s" default-directory)
  ;; Need to use `expand-file-name` to expand `~` into a full path
  ;; Otherwise, termhere fallback to `$HOME`
  ;; The Rust version of `termhere' only works with `call-process-shell-command',
  ;; `async-shell-command', and `shell-command'. But the (b)ash version works
  ;; out of the box. Including with `start-process'.
  ;; See https://github.com/azzamsa/dotfiles/blob/master/xtool/src/termhere.rs
  (call-process-shell-command (concat "termhere " (expand-file-name default-directory))))
