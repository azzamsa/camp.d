;; ;; -*- lexical-binding: t; -*-


;;;###autoload
(defun +log (msg &rest vars)
  "Log MSG and VARS using `message' when `init-file-debug' is non-nil."
  (when init-file-debug
    (apply #'message (cons (concat "[Camp:Log] " msg) vars))))

 ;;;###autoload
(defun +info (msg &rest vars)
  "Log info MSG and VARS using `message'."
  (let ((inhibit-message))
    (apply #'message (cons (concat "[Camp:Info] " msg) vars))))

(defmacro +cmd! (&rest body)
  "Returns (lambda () (interactive) ,@body)
A factory for quickly producing interaction commands, particularly for keybinds
or aliases."
  (declare (doc-string 1) (pure t) (side-effect-free t))
  `(lambda (&rest _) (interactive) ,@body))

(defmacro +cmd!! (command &optional prefix-arg &rest args)
  "Returns a closure that interactively calls COMMAND with ARGS and PREFIX-ARG.
Like `cmd!', but allows you to change `current-prefix-arg' or pass arguments to
COMMAND. This macro is meant to be used as a target for keybinds (e.g. with
`define-key' or `map!')."
  (declare (doc-string 1) (pure t) (side-effect-free t))
  `(lambda (arg &rest _) (interactive "P")
     (let ((current-prefix-arg (or ,prefix-arg arg)))
       (,(if args
             #'funcall-interactively
           #'call-interactively)
        ,command ,@args))))

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
