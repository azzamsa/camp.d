;; -*- lexical-binding: t; -*-

;; Icons
(use-package all-the-icons
  :straight t)

(use-package svg-lib
  :straight t
  :defer t
  :custom
  (svg-lib-icons-dir (expand-file-name "svg-lib" camp-cache-dir))) ; Change cache dir

;; Themes
(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-dracula t))

;; Modeline
(use-package doom-modeline
  :straight t
  :config
  (setq doom-modeline-bar-width 5
        doom-modeline-height 37)
  (doom-modeline-mode 1))

(use-package visual-fill-column
  :straight t
  :custom
  (visual-fill-column-width nil)
  (visual-fill-column-center-text t))

(use-package minimap
  :straight t
  :after camp-loaded
  :defer t
  :config
  (setq minimap-window-location 'right
        minimap-update-delay 0
        minimap-width-fraction 0.09
        minimap-minimum-width 15))

(use-package writeroom-mode
  :straight t
  :defer t
  :after camp-loaded
  :config
  (setq writeroom-width 90))

;;;###autoload
(defalias '+zen/toggle #'writeroom-mode)

(defvar +zen--last-wconf nil)
;;;###autoload
(defun +zen/toggle-fullscreen ()
  "Toggle `writeroom-mode' fullscreen and delete all other windows.
Invoke again to revert to the window configuration before it was activated."
  (interactive)
  (require 'writeroom-mode)
  (let ((writeroom-global-effects +zen--old-writeroom-global-effects)
        (writeroom-maximize-window t))
    (if writeroom-mode
        (progn
          (set-frame-parameter
           nil 'fullscreen
           (let ((fullscreen-restore (frame-parameter nil 'fullscreen-restore)))
             (if (memq fullscreen-restore '(maximized fullheight fullwidth))
                 fullscreen-restore
               nil)))
          (set-window-configuration +zen--last-wconf))
      (setq +zen--last-wconf (current-window-configuration))
      (modify-frame-parameters
       nil `((fullscreen . fullboth)
             (fullscreen-restore . ,(frame-parameter nil 'fullscreen)))))
    (let ((writeroom-global-effects (remq 'writeroom-set-fullscreen writeroom-global-effects)))
      (call-interactively #'+zen/toggle))))

(use-package dashboard
  :straight t
  :after evil evil-collection
  :demand t
  :init
  (defun aza/dashboard-insert-quote (list-size)
    "Inserts a random non-comment quote from the 'quotes' file into the dashboard."
    (dashboard-insert-heading "Quote of the Day:" nil (all-the-icons-faicon "commenting-o" :face 'dashboard-heading))
    (insert "\n")
    (let* ((lines (with-temp-buffer
                    (insert-file-contents (concat camp-etc-dir "quotes"))
                    (split-string (buffer-string) "\n" t)))
           (filtered-lines (cl-remove-if (lambda (line) (string-match-p "^\\s-*#" line)) lines))
           (random-line (when filtered-lines
                          (string-join (split-string (nth (random (length filtered-lines)) filtered-lines)) " "))))
      (insert "    " random-line)))
  :custom
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-center-content t)
  (dashboard-banner-ascii "Camp")
  (dashboard-banner-logo-title "Want to go camping?")
  (dashboard-items '((recents . 5) (projects . 5) (bookmarks . 5)))
  (dashboard-image-banner-max-width 600)
  (dashboard-projects-backend 'project-el)
  (dashboard-startup-banner (concat user-emacs-directory "docs/logo.png"))
  (dashboard-items '((daily-quote)
                     (recents . 5)
                     (projects . 5)
                     (bookmarks . 5)))
  (dashboard-item-generators '((daily-quote . aza/dashboard-insert-quote)
                               (recents . dashboard-insert-recents)
                               (projects . dashboard-insert-projects)
                               (bookmarks . dashboard-insert-bookmarks)))
  :config
  ;; Ensure setting the keybindings before opening the dashboard
  (evil-collection-dashboard-setup)

  ;; Avoid opening the dashboard when Emacs starts with an open file.
  (unless (cl-some #'buffer-file-name (buffer-list))
    (dashboard-open)))

(provide 'camp-ui)
