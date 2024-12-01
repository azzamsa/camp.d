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

(defvar +quotes
  '("🎨 Practice. It is Practice. Practice"  ;; Sarah Andersen
    "😱 Do it scared"
    "✅ Done is better than perfect"
    "🌱 Embrace Progress over Achievement"
    "📌 Do first things first"
    "🔄 We are what we repeatedly do"
    "🧩 A problem well-put is half-solved"
    "🤔 Decide to decide what not to do"
    "💪 Failure is success in progress"
    "🦶 One step at a time"
    "💯 Do your best"
    "💥 Action cures fear"
    "⏳ It's too early to tell"
    "🤝 Make customer, not sale"
    "🎯 A goal without plan is a wish"))

(provide 'camp-core)
