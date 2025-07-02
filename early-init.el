;;; early-init.el -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)

(setq load-prefer-newer t)
;;; Garbage collection
;; Increase the GC threshold for faster startup
(setq gc-cons-threshold (* 64 1024 1024))

;; Add directories to `load-path'
(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))

(require 'camp-vars)
(require 'camp-utils)

;;; Write user custom variables to separate file instead of init.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;;; UI configuration
;; Remove some unneeded UI elements (the user can turn back on anything they wish)
(setq inhibit-startup-message t)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(mouse-color . "blue") default-frame-alist)
;; No hostname in frame title
;; Without setting the `icon-title-format`. The window title will revert
;; back to its original value after losing its focus.
(setq frame-title-format '("" invocation-name " - " "%b"))
(setq icon-title-format '("" invocation-name " - " "%b"))

;; Make the initial buffer load faster by setting its mode to fundamental-mode
(customize-set-variable 'initial-major-mode 'text-mode)
