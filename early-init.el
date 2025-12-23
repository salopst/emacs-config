;;; ~/.config/emacs-new/early-init.el --- Slim & modern 2025 config -*- lexical-binding: t -*-

;; Last modified: 2025-11-30

;; some speed up tips from doom
(defvar default-gc-cons-threshold 16777216 ; 16mb
  "my default desired value of `gc-cons-threshold'
during normal emacs operations.")

;; make garbage collector less invasive
(setq gc-cons-threshold  most-positive-fixnum
  gc-cons-percentage 0.6)
;; Don’t compact font caches during GC.
(setq inhibit-compacting-font-caches t)

;;; UI

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq inhibit-startup-message t)

;; start the initial frame maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; start every frame maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Tell emacs where is your personal elisp lib dir
(add-to-list 'load-path "~/.config/emacs/sjy2-lisp/")
(add-to-list 'load-path "~/.config/emacs/git-cloned-lisp/")


;; Another tip from doom.
(setq default-file-name-handler-alist file-name-handler-alist
  file-name-handler-alist nil)

;; And then finally a hook to reset everything.
(add-hook 'emacs-startup-hook
  (lambda (&rest _)
    (setq gc-cons-threshold default-gc-cons-threshold
          gc-cons-percentage 0.1
          file-name-handler-alist default-file-name-handler-alist)
    ;; delete no longer necessary startup variable
    (makunbound 'default-file-name-handler-alist)))

