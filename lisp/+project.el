;; -*- lexical-binding: t; -*-

;;
;; Commands
;;

;;;###autoload
(defun camp/browse-in-emacsd ()
  "Browse files from `user-emacs-directory'."
  (interactive) (camp-project-browse user-emacs-directory))

;;;###autoload
(defun camp/find-file-in-emacsd ()
  "Find a file under `user-emacs-directory', recursively."
  (interactive) (camp-project-find-file user-emacs-directory))

;;
;; Library
;;


(defun camp-project-find-file (dir)
  "Jump to a file in DIR (searched recursively). "
  (let* ((default-directory (file-truename dir))
         (pr (+project-from-dir dir))
         (root (project-root pr))
         (dirs (list root)))
    (if pr
        (project-find-file-in nil dirs pr nil)
      (call-interactively #'find-file))))

(defun camp-project-browse (dir)
  "Traverse a file structure starting linearly from DIR."
  (let ((default-directory (file-truename (expand-file-name dir))))
    (call-interactively #'find-file)))
