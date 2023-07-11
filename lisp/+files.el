;; -*- lexical-binding: t; -*-

;;;###autoload
(defun +ensure-directory (&rest path-parts)
  "Concatenate PATH-PARTS to construct a path and return it.

Ensure the path exists, if not create it. The exact behavior is to create the
parent directory if the path is a file, and if the path is a directory, create
that directory."
  (let* ((path (mapconcat #'identity path-parts nil))
         (parent-dir (file-name-directory path)))
    (unless (file-directory-p parent-dir)
      (ignore-errors (mkdir parent-dir t))
      (unless (file-directory-p parent-dir)
        (+error! "Cannot create directory %s" parent-dir)))
    path))
