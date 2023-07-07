;;;###autoload
(defun doom/window-enlargen (&optional arg)
  "Enlargen the current window (i.e. shrinks others) so you can focus on it.
Use `winner-undo' to undo this. Alternatively, use
`delete-other-windows'."
  (interactive "P")
  (let* ((window (selected-window))
         (dedicated-p (window-dedicated-p window))
         (preserved-p (window-parameter window 'window-preserved-size))
         (ignore-window-parameters t)
         (window-resize-pixelwise nil)
         (frame-resize-pixelwise nil))
    (unwind-protect
        (progn
          (when dedicated-p
            (set-window-dedicated-p window nil))
          (when preserved-p
            (set-window-parameter window 'window-preserved-size nil))
          (maximize-window window))
      (set-window-dedicated-p window dedicated-p)
      (when preserved-p
        (set-window-parameter window 'window-preserved-size preserved-p)))))
