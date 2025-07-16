;; -*- lexical-binding: t; -*-

;;
;; Icons

;; Nerd Font icons for Emacs
(use-package nerd-icons
  :ensure t)

;; A fancy and fast mode-line inspired by minimalism design
(use-package doom-modeline
  :ensure t
  :config
  (setq doom-modeline-bar-width 5
        doom-modeline-height 37
        doom-modeline-buffer-encoding nil)
  (doom-modeline-mode 1))

;; Display typographical ligatures in major modes
(use-package ligature
  :ensure t
  :hook (prog-mode . ligature-mode)
  :when (and (featurep 'feat/harfbuzz) (featurep 'feat/cairo) (version<= "1.16.0" cairo-version-string))
  :config
  :config
  ;; A fine-tuned list of per-language ligatures, constructed from:
  ;; - Iosevka: https://typeof.net/Iosevka/customizer
  ;; - Fira Code: https://github.com/tonsky/FiraCode/wiki/Emacs-instructions
  ;; - Cascadia Code: https://github.com/microsoft/cascadia-code/wiki/Coding-ligature-coverage
  (defvar +ligature-common-prog
    `("<<" "<<<" ">>>" ">>" ">>=" "<<=" "<=" ">=" "::" ":::" "..=" "::<" "=="
      "*=" "+=" "<|" "<|>" "|>" "++" "+++" "&&" "||" "/=" "--" "#!" "::="
      "#[" "]#" "{|" "|}" "__"))

  (defvar +ligature-c-like ; C, C++, C#, Java, Rust, JS, PHP, Go, V
    `("!=" "<>" "/*" "*/" "//" "///" "^=" "|=" "?." "??" "<~>"))

  (defvar +ligature-html ; HTML, XML, JS, PHP
    `("</" "</>" "/>" "<!--" "<!---" "www"))

  (defvar +ligature-brackets ; Ruby, PHP, Julia, ML, Haskell, Raku, Dafny, Swift, Idris, PHP
    `("<>" "<:" ":=" "*+" "<*" "<*>" "*>" "<." "<.>" ".>" "+*" "=*"
      "=:" ":>" "(*" "*)" "/*" "*/"))

  (defvar +ligature-js `(,@+ligature-c-like ,@+ligature-html "!==" "!===" "==="))

  (defvar +ligature-lisp `(";;" ";;;"))

  (defvar +ligature-markdown
    `("##" "###" "####" "#####" "######" "--" "---" "----" "-----" "------"))

  (defvar +ligature-functional ; ML, Ocaml, F#, Idris, Coq, Haskell, Elm, PureScript
    `(,@+ligature-brackets "~~" "~-" "<>" "\\/" "/\\" "|-" "-|" "[|" "|]" "%%" "<$" "<$>" "$>" "=/="))

  (defvar +ligature-arrows
    `("<-" "->" "<<-" "->>" "<--" "-->" "<---" "--->"
      "=>" "<==" "==>" "<===" "===>" "<<=" "=>>" "<<==" "==>>" "<->" "<=>"
      "<~~" "~~>" "<-->" "<--->" "<---->" "<==>" "<===>" "<====>"))

  (defvar +ligature-arrows-extra
    '("-<<" "-<" "-<-" "->-" ">-" ">>-" "=<<" "=<" "=<=" "=>="
      "<<==>>" "|-|-|" "|=|=|" "/=/" "=<<=" "=>>=" "-<<-" "->>-" "||-" "-||"
      "<=//" "//=>" "<=|" "|=>" "<-|" "|->" "<-<<" ">>->" "<=<<" ">>=>"
      "__|__" "/==/==/" "//==//==//" "|==|==|" "||==||==||" "<==<==<" ">==>==>"
      "<<==<<==<<" ">>==>>==>>" "|--|--|" "||--||--||" "<--<--<" ">-->-->"
      "<<--<<--<<" ">>-->>-->>"))

  (ligature-set-ligatures 't '("ff" "ffi" "Fl" "Tl" "fi" "fj" "fl" "ft" "www"))
  (ligature-set-ligatures '(prog-mode conf-mode) `(,@+ligature-common-prog ,@+ligature-arrows ,@+ligature-arrows-extra))
  (ligature-set-ligatures '(text-mode) `(,@+ligature-arrows ,@+ligature-arrows-extra))
  (ligature-set-ligatures '(js-mode typescript-mode typescript-ts-mode php-ts-mode php-mode) +ligature-js)
  (ligature-set-ligatures '(julia-mode julia-ts-mode ess-julia-mode ruby-mode ruby-ts-mode php-mode) +ligature-brackets)
  (ligature-set-ligatures '(markdown-mode markdown-ts-mode) +ligature-markdown)
  (ligature-set-ligatures '(html-mode nxml-mode) +ligature-html)
  (ligature-set-ligatures
   '( emacs-lisp-mode lisp-mode lisp-data-mode common-lisp-mode
      hy-mode scheme-mode geiser-mode)
   +ligature-lisp)
  (ligature-set-ligatures
   '( c-mode c++-mode opencl-c-mode cuda-mode llvm-ts-mode java-mode
      java-ts-mode csharp-mode csharp-ts-mode rust-mode rust-ts-mode
      go-mode go-ts-mode go-mod-ts-mode v-mode v-ts-mode zig-mode zig-ts-mode)
   +ligature-c-like)
  (ligature-set-ligatures
   '( haskell-mode haskell-ts-mode elm-mode elm-ts-mode purescript-mode
      purescript-ts-mode ml-mode caml-mode tuareg-mode fsharp-mode fstar-mode
      fsharp-ts-mode dafny-mode swift-mode coq-mode idris-mode)
   +ligature-functional))

(use-package visual-fill-column
  :ensure t
  :custom
  (visual-fill-column-width nil)
  (visual-fill-column-center-text t))

(use-package minimap
  :ensure t
  :defer t
  :config
  (setq minimap-window-location 'right
        minimap-update-delay 0
        minimap-width-fraction 0.09
        minimap-minimum-width 5))

(use-package writeroom-mode
  :ensure t
  :defer t
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
  :ensure t
  :demand t
  :init
  (defun camp-dashboard-insert-quote (list-size)
    "Insert a random quote into the dashboard."
    (dashboard-insert-heading "Quote of the Day:" nil (nerd-icons-faicon "nf-fa-commenting_o" :face 'dashboard-heading))
    (insert "\n")
    (when +quotes
      (let ((random-quote (nth (random (length +quotes)) +quotes)))
        (insert "    " (propertize random-quote 'face 'bold) "\n"))))
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
  (dashboard-item-generators '((daily-quote . camp-dashboard-insert-quote)
                               (recents . dashboard-insert-recents)
                               (projects . dashboard-insert-projects)
                               (bookmarks . dashboard-insert-bookmarks)))
  :config
  (setq dashboard-icon-type 'nerd-icons)

  ;; Ensure setting the keybindings before opening the dashboard
  (evil-collection-dashboard-setup)

  ;; Avoid opening the dashboard when Emacs starts with an open file.
  (unless (cl-some #'buffer-file-name (buffer-list))
    (dashboard-open)))

(provide 'camp-ui)
