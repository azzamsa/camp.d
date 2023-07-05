;; ;; -*- lexical-binding: t; -*-


;;;###autoload
(defun +log (msg &rest vars)
  "Log MSG and VARS using `message' when `init-file-debug' is non-nil."
  (when init-file-debug
    (apply #'message (cons (concat "[Camp] " msg) vars))))

 ;;;###autoload
(defun +info (msg &rest vars)
  "Log info MSG and VARS using `message'."
  (let ((inhibit-message))
    (apply #'message (cons (concat "[Camp] " msg) vars))))

;;;###autoload
(defun +set-fonts ()
  "Set Emacs' fonts from `camp-fonts'."
  (interactive)
  (custom-set-faces
   `(default
     ((t (:font ,(format "%s %d"
                         (or (plist-get camp-fonts :font-family)
                             (plist-get camp-default-fonts :font-family))
                         (or (plist-get camp-fonts :font-size)
                             (plist-get camp-default-fonts :font-size)))))))
   `(fixed-pitch
     ((t (:inherit (default)))))
   `(fixed-pitch-serif
     ((t (:inherit (default)))))
   `(variable-pitch
     ((t (:font ,(format "%s %d"
                         (or (plist-get camp-fonts :variable-pitch-font-family)
                             (plist-get camp-default-fonts :variable-pitch-font-family))
                         (or (plist-get camp-fonts :variable-pitch-font-size)
                             (plist-get camp-default-fonts :variable-pitch-font-size))))))))
  ;; Run hooks
  (run-hooks 'camp-after-set-fonts-hook))

(provide 'camp-utils)
