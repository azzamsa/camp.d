;; -*- lexical-binding: t; -*-

(use-package tabspaces
  :straight t
  :after camp-loaded
  :hook (tabspaces-mode . +consult-tabspaces-setup)
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Main")
  :init
  (+map! :infix "TAB"
    "TAB" '(tabspaces-switch-or-create-workspace   :wk "Switch or create")
    "d"   '(tabspaces-close-workspace              :wk "Close workspace")
    "k"   '(tabspaces-kill-buffers-close-workspace :wk "Kill buffers & close workspace"))
  :config
  (defun +consult-tabspaces-setup ()
    "Deactivate isolated buffers when not using tabspaces."
    (require 'consult)
    (cond (tabspaces-mode
           ;; hide full buffer list (still available with "b")
           (consult-customize consult--source-buffer :hidden t :default nil)
           (add-to-list 'consult-buffer-sources '+consult--source-workspace))
          (t
           ;; reset consult-buffer to show all buffers
           (consult-customize consult--source-buffer :hidden nil :default t)
           (setq consult-buffer-sources (remove #'+consult--source-workspace consult-buffer-sources)))))

  (with-eval-after-load 'consult
    ;; Hide full buffer list (still available with "b" prefix)
    (consult-customize consult--source-buffer :hidden t :default nil)
    ;; Set consult-workspace buffer list
    (defvar +consult--source-workspace
      (list :name "Workspace Buffers"
            :narrow   '(?w . "Workspace")
            :history  'buffer-name-history
            :category 'buffer
            :state    #'consult--buffer-state
            :default  t
            :items
            (lambda ()
              (consult--buffer-query
               :predicate #'tabspaces--local-buffer-p
               :sort      'visibility
               :as        #'buffer-name))))
    (add-to-list 'consult-buffer-sources '+consult--source-workspace))

  (tabspaces-mode 1)

  ;; Rename the first tab to `tabspaces-default-tab'
  (tab-bar-rename-tab tabspaces-default-tab))

(provide 'camp-workspaces)
