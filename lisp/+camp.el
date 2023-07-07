;; -*- lexical-binding: t; -*-

;;;###autoload
(defun camp-scratch-buffer ()
  "Toggle persistent scratch buffer"
  (interactive)
  (let ((filename camp-scratch-file))
    (if-let ((buffer (find-buffer-visiting filename)))
        (if (eq (selected-window) (get-buffer-window buffer))
            (delete-window)
          (if (get-buffer-window buffer)
              (select-window (get-buffer-window buffer))
            (pop-to-buffer buffer)))
      (progn
        (split-window-vertically)
        (other-window 1)
        (find-file filename)))))
