;;; ~/.config/emacs-new/init.el --- v1.5 • Dec 07 2025 -*- lexical-binding: t -*-

;;; Commentary:
;; Final slim config — everything you actually want from the old monster,
;; cleanly merged into our modern 2025 base.
;; mediated by grok 3.x

;; code ~/.config/emacs-old/init.el ~/.config/emacs-q/init.el ~/.config/emacs/init.el


;;; SOME KEYBINDS
;; C-M-f → forward-sexp
;; C-M-b → backward-sexp
;; C-M-k → kill-sexp
;; C-M-t → transpose-sexps
;; C-M-<SPC> (mark-sexp) repeatedly extends the selection to larger syntactic units. 

;;; my NEW keybinds:
;; C-c n f  → sjy2/consult-denote-search      (find any note)
;; C-c n g  → sjy2/consult-denote-grep        (grep note contents)
;; C-c n r  → sjy2/denote-quick-reference     (quick ref lookup)
;; C-c n k  → sjy2/denote-keyword-filter      (filter by keyword)
;; C-c w w  → sjy2/writing-session            (focused writing)

;;; Code:

;;; ———————————————————————— 00 Directory setup & Local Lisp ————————————————————————
;;(defconst alt-emacs-dir "~/.config/emacs-old")  ;  old bloated config root
(defconst sjy2-cache-dir      (expand-file-name "sjy2-cache/"      user-emacs-directory))
(defconst sjy2-etc-dir        (expand-file-name "sjy2-etc/"        user-emacs-directory))
(defconst sjy2-lisp-dir       (expand-file-name "sjy2-lisp/"       user-emacs-directory))
(defconst sjy2-themes-dir     (expand-file-name "themes/"          user-emacs-directory))
(defconst git-cloned-lisp-dir (expand-file-name "git-cloned-lisp/" user-emacs-directory))

;; Create essential directories if they don't exist
(dolist (dir (list sjy2-cache-dir sjy2-etc-dir sjy2-lisp-dir))
  (unless (file-directory-p dir)
    (make-directory dir t)))

;; Add personal Lisp directories to load-path
(dolist (dir (list sjy2-lisp-dir git-cloned-lisp-dir))
  (add-to-list 'load-path dir))

;; Add user themes directory (and immediate subdirectories) to custom-theme-load-path
(when (file-directory-p sjy2-themes-dir)
  (add-to-list 'custom-theme-load-path sjy2-themes-dir)
  (dolist (sub (directory-files sjy2-themes-dir t directory-files-no-dot-files-regexp))
    (when (file-directory-p sub)
      (add-to-list 'custom-theme-load-path sub))))

;;; ———————————————————————— 01 Bootstrap use-package ————————————————————————
(require 'package)
(setq package-archives '(("elpa"  . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t
      use-package-verbose t
      use-package-compute-statistics t)

;;; ———————————————————————— 015 my packages ————————————————————————

;; my shit customizations
(use-package sjy2-custom-code
  :load-path "~/.config/emacs/sjy2-lisp/"
  :demand t
  :config
  (message "sjy2-custom-code loaded"))

;;; AI, LLM, ChatGPT etc.
(use-package sjy2-gptel
  :load-path (lambda () (list sjy2-lisp-dir))
  :defer t 
  :config
  (message "sjy2-gptel loaded"))

;;; TODO : Stick all the direds in this file?
(use-package sjy2-dired-open-with
  :load-path "~/.config/emacs/sjy2-lisp/"
  :demand t
  :config
  (message "sjy2-dired-open-with loaded"))

(require 'eros)

;;; ———————————————————————— 02 Core state files ————————————————————————
(setq save-place-file              (expand-file-name "save-place"         sjy2-cache-dir))
(setq desktop-base-file-name       (expand-file-name "desktop"            sjy2-cache-dir))
(setq recentf-save-file            (expand-file-name "recentf"            sjy2-cache-dir))
(setq bookmark-default-file        (expand-file-name "bookmarks"          sjy2-cache-dir))
(setq savehist-file                (expand-file-name "savehist"           sjy2-cache-dir))
(setq tramp-persistency-file-name  (expand-file-name "tramp"              sjy2-cache-dir))
(setq project-list-file            (expand-file-name "project-list.el"    sjy2-cache-dir))
(setq eshell-history-file-name     (expand-file-name "eshell/history"     sjy2-cache-dir))
(setq mc/list-file                 (expand-file-name ".mc-lists.el"       sjy2-cache-dir))
(setq abbrev-file-name             (expand-file-name "abbrev_defs"        sjy2-etc-dir))
(setq custom-file                  (expand-file-name "custom.el"          sjy2-etc-dir))


;; (setopt org-directory    (expand-file-name "org/"        sjy2-etc-dir))
;; (setopt denote-directory (expand-file-name "denote/"     sjy2-etc-dir))
;; (setopt tempel-path      (expand-file-name "tempel-snippets.el" sjy2-etc-dir))

(setq custom-file (expand-file-name "custom.el" sjy2-etc-dir))
(load custom-file 'noerror)

(setq auto-save-visited-interval 30
      auto-save-default t
      auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save/" sjy2-cache-dir) t))

      backup-directory-alist `(("." . ,(expand-file-name "backups/" sjy2-cache-dir)))
      make-backup-files t backup-by-copying t version-control t
      kept-new-versions 10 kept-old-versions 2 delete-old-versions t)

;;; ———————————————————————— 03 Modern essentials  ————————————————————————
(setq inhibit-startup-screen t)
(setq visible-bell t)
(setq warning-minimum-level :error)           ; ← sane, not :emergency
(setq create-lockfiles nil)
(setq initial-scratch-message ";;; SCRATCH! Ah-ha!\n;;; Buffer of the Universe !!!!!!\n\n")
(setq custom-safe-themes          t)
(setq custom-safe-variables       t)
(setq desktop-load-locked-desktop t)  ; ignore lock from previous invocation


(add-to-list 'default-frame-alist '(fullscreen . maximized))

(tool-bar-mode  -1)
(menu-bar-mode  -1)
(tooltip-mode   -1)
(scroll-bar-mode 1)
(tab-bar-mode   -1)                              ; we don’t use Emacs tabs
(fringe-mode    15)
(pixel-scroll-precision-mode 1)
(setq pixel-scroll-precision-interpolate-page t)

(delete-selection-mode 1)
(repeat-mode 1)
(save-place-mode 1)
(savehist-mode 1)
(recentf-mode 1)
(delete-selection-mode             1)  ; typing replaces the selection
(desktop-save-mode                 1)  ; remember last opened files
(global-auto-revert-mode 1)
(setq auto-revert-interval 0.5)
(setq global-auto-revert-non-file-buffers t)

(column-number-mode 1)
(global-hl-line-mode 1)
(setq use-short-answers t)
(setq delete-by-moving-to-trash t)
(setq indent-tabs-mode nil)
(setq tab-always-indent 'complete)
(setq display-line-numbers       'absolute)
(setq tab-width 2)
(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq completion-cycle-threshold 3)
(setq completions-detailed       t)
(setq mark-ring-max              69)
(setq global-mark-ring-max       69)
(setq bookmark-save-flag         1)
(setq savehist-additional-variables '(kill-ring register-alist))
(setq save-interprogram-paste-before-kill t)
(setq recentf-max-saved-items   500)
(setq recentf-max-menu-items    100)
(setq recentf-max-saved-items   100)
(setq recentf-auto-cleanup      nil)
(setq recentf-save-file-modes   nil)

(setq split-width-threshold     1)           ; prefer side-by-side splits
(setq scroll-error-top-bottom   t)
(setq scroll-preserve-screen-position       t)
(setq isearch-repeat-on-direction-change    t)
(setq switch-to-buffer-obey-display-actions t)

;; Line numbers only in programming modes
(global-display-line-numbers-mode -1)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Visual line mode in text modes
(add-hook 'text-mode-hook #'visual-line-mode)

;; Variable-pitch in a few readable modes
(dolist (hook '(text-mode-hook
                org-mode-hook
                markdown-mode-hook
                latex-mode-hook))
  (add-hook hook #'variable-pitch-mode))

(setq-default cursor-type 'box)     ; As RMS intended
(add-to-list 'default-frame-alist '(cursor-type . box))

(defun sjy2/force-box-cursor ()
  "Aggressively force box cursor."
  (interactive)
  (setq cursor-type 'box)
  (when (called-interactively-p 'interactive)
    (message "Cursor forced to box")))

(add-hook 'post-command-hook                #'sjy2/force-box-cursor)
(add-hook 'focus-in-hook                    #'sjy2/force-box-cursor)
(add-hook 'buffer-list-update-hook          #'sjy2/force-box-cursor)
(add-hook 'window-configuration-change-hook #'sjy2/force-box-cursor)
(with-eval-after-load 'org
  (add-hook 'org-mode-hook #'sjy2/force-box-cursor 99))

(global-set-key (kbd "C-c C-b") #'sjy2/force-box-cursor)

;;; ———————————————————————— 04 Keybindings ————————————————————————
(keymap-set key-translation-map "ESC" "C-g")
(keymap-global-set "C-="        #'text-scale-increase)
(keymap-global-set "C--"        #'text-scale-decrease)
(keymap-global-set "C-z"        #'undo)
(keymap-global-set "C-x g"      #'magit-status)
(keymap-global-set "C-/"        #'comment-line)
(keymap-global-set "C-x C-m"    #'execute-extended-command)
(keymap-global-set "C-c C-m"    #'execute-extended-command)

(keymap-global-set "C-="        #'text-scale-increase)
(keymap-global-set "C--"        #'text-scale-decrease)
(keymap-global-set "C-v"        #'yank) ; from CUA; was scroll-up; default C-y
(keymap-global-set "C-x C-r"    #'eval-print-last-sexp)
(keymap-global-set "C-s"        #'isearch-forward-regexp)
(keymap-global-set "C-r"        #'isearch-backward-regexp) ; M-e in isearch -- allows proper editing
(keymap-global-set "C-x C-g"    #'find-file-at-point)
(keymap-global-set "C-c i m"    #'imenu)
(keymap-global-set "C-M-r"      #'query-replace-regexp)    ; C-M-% is the finger crunching default
(keymap-global-set "C-c t f"    #'auto-fill-mode)
(keymap-global-set "C-c t l"    #'toggle-truncate-lines)
(keymap-global-set "C-c t w"    #'white-space-mode)
(keymap-global-set "C-/"        #'comment-line) ; C-/ is undo by default. C-x C-; al comm. line
(keymap-global-set "C-c c"      #'org-capture)
(keymap-global-set "C-c a"      #'org-agenda)
(keymap-global-set "C-c l"      #'org-store-link)
;; WARNING -- external dependencies
(keymap-global-set "C-c t t"    #'sjy2/toggle-transparency)
(keymap-global-set "C-g"        #'prot/keyboard-quit-dwim)
(keymap-global-set "C-x o"      #'sjy2/cycle-windows-and-frames)
(keymap-global-set "C-x C-r"    #'recentf-open) ; override `find-file-read-only'
(keymap-global-set "M-u"        #'sjy2/cycle-case-region-or-word)
(keymap-global-set "C-c d"      #'duplicate-dwim) ; Emacs default version of the crux
;;(keymap-global-set "C-c d"      #'crux-duplicate-current-line-or-region)
(keymap-global-set "C-c D D"    #'crux-delete-file-and-buffer)
(keymap-global-set "C-c o w"    #'crux-open-with) ; xdg-open
(keymap-global-set "C-c o r"    #'crux-reopen-as-root)
(keymap-global-set "M-r"        #'crux-recentf-find-file) ; recentf-list void 
(keymap-global-set "s-o"        #'crux-smart-open-line)
(keymap-global-set "s-O"        #'crux-smart-open-line-above)
(global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
(global-set-key [remap kill-whole-line]        #'crux-kill-whole-line)
(with-eval-after-load 'org
  (keymap-set org-mode-map "M-s-<up>" #'org-move-subtree-up)
  (keymap-set org-mode-map "M-s-<down>" #'org-move-subtree-down))

(global-set-key (kbd "C-x C-d") 'dired) ; is usually list-directory
;; C-j is usually bound to org-return-and-maybe-indent in org-mode
;; C-j is usually bound to electric-newline-and-maybe-indent in elisp mode

(defun my-override-c-j-in-mode ()
  (define-key (current-local-map) (kbd "C-j") #'avy-goto-char-timer))
(add-hook 'emacs-lisp-mode-hook #'my-override-c-j-in-mode)

(with-eval-after-load "org"
  (define-key org-mode-map (kbd "C-j") #'avy-goto-char-timer) ; default = org-return-and-maybe-indent
  (define-key org-mode-map (kbd "C-'") #'embrace-commander) ; default = org-cycle-agenda-files 
  )

(keymap-global-set "M-S-<up>"   #'org-move-subtree-up)
(keymap-global-set "M-S-<down>" #'org-move-subtree-down)

;; C-w -- Steve Yegge's classic backwards-kill-word rewritten
(define-advice kill-region (:around (orig-fun beg end &rest args) sjy2/unix-werase)
  "If called interactively with no active region, delete one word backward.
Otherwise call `kill-region` as usual."
  (if (or (use-region-p)
          (not (called-interactively-p 'interactive)))  ; <-- KEY FIX
      ;; Region active OR called non-interactively --> normal kill-region
      (apply orig-fun beg end args)
    ;; Called interactively with no region --> behave like Unix werase
    (let ((p (point))
          (q (save-excursion (backward-word 1) (point))))
      (kill-new (buffer-substring-no-properties q p))
      (delete-region q p))))


;; M-w = copy line when no region (like VS Code C-c) — perfect 2025 version
(defun sjy2/kill-ring-save-dwim (&optional arg)
  "Copy region if active, otherwise copy current line.
With prefix ARG, copy the line with trailing newline (like `kill-line')."
  (interactive "P")
  (if (use-region-p)
      (kill-ring-save (region-beginning) (region-end))
    (let ((beg (line-beginning-position))
          (end (line-end-position)))
      (when arg
        (setq end (min (point-max) (1+ end))))  ; include newline
      (kill-new (buffer-substring-no-properties beg end))
      (message "Copied line%s" (if arg " (with newline)" "")))))

;; Replace M-w globally with the DWIM version
(keymap-global-set "M-w" #'sjy2/kill-ring-save-dwim)

;;; ———————————————————————— 05 Modeline, Appearance, Themes, Fonts ————————————————————————
(set-face-attribute 'default nil :font "Iosevka" :height 140) ; Set default face font and height
(set-frame-parameter nil 'font "Iosevka-14") ; Set initial frame font (for good measure)

;; modeline
(use-package doom-modeline
  ;; :vc (:url "https://github.com/seagle0128/doom-modeline.git")
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-icon t)
  (doom-modeline-lsp t)                          ; LSP icon
  (doom-modeline-minor-modes t)                  ; nil default. t for minions mode
  (setopt doom-modeline-buffer-file-name-style 'truncate-nil) ;; 'auto
  (doom-modeline-mode t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-height 30)
  (doom-modeline-bar-width 6))


;; replaces minions for Emacs 31+ ???
;; (setq mode-line-collapse-minor-modes t)
(use-package minions
  ;; :vc (:url "https://github.com/tarsius/minions.git")
  :ensure t
  :hook (after-init . minions-mode)
  :custom
  (minions-mode-line-lighter " 🛠️ ")
  (minions-direct '(eglot-mode flymake-mode))
  (minions-prominent-modes '(projectile-mode eglot-mode)))

(use-package keycast
  :ensure t
  :hook (after-init . keycast-header-line-mode)   ; just turn it on
  :custom
  (keycast-substitute-alist
   '((self-insert-command . "")))       ; ignore typing letters/numbers
  (keycast-separator-width 20)
  (keycast-hide-timeout 5000))

;; ---------------------- THEMES
;; M-x list-faces-display -- see everything
;; M-x describe-face

;; ;; --- NEW GRUBER-DARKER MAPPINGS (for Vivendi overrides) ---
;;     (gd-bg-base             . "#181818")   ; gruber-darker-bg
;;     (gd-fg-base             . "#e4e4ef")   ; gruber-darker-fg
;;     (gd-dim-bg              . "#282828")   ; gruber-darker-bg+1 (for dim/alt/diffs)
;;     (gd-active-bg           . "#453d41")   ; gruber-darker-bg+2 (for region/active lines)
;;     (gd-red                 . "#f43841")   ; gruber-darker-red
;;     (gd-green               . "#73c936")   ; gruber-darker-green
;;     (gd-yellow              . "#ffdd33")   ; gruber-darker-yellow
;;     (gd-brown               . "#cc8c3c")   ; gruber-darker-brown (for comments)
;;     (gd-quartz              . "#95a99f")   ; gruber-darker-quartz (for constants/fn names)
;;     ))

(defconst sjy2-modus-palette
  '(
    ;; Core Light Accents
    (sjy2-bg                . "#FCFAF0")
    (sjy2-bg-1              . "#E4E3CF")
    (sjy2-bg-2              . "#C3C6C7")
    (sjy2-fg                . "#181818") ; #282828
    (sjy2-fg-1              . "#9fa2a3")
    (sjy2-fg-2              . "#616161")
    (sjy2-error             . "#F15952")
    (sjy2-warning           . "#F8BB7C")
    (sjy2-success           . "#87CF70")
    (sjy2-quartz            . "#95A99F")   ; comment
    (sjy2-eyes              . "#4B919E")   ; constant, fnname
    (sjy2-wisteria          . "#9e95c7")   ; keyword
    (sjy2-niagara           . "#96a6c8")
    (sjy2-shrews            . "#2e5292")   ; cursor
    (sjy2-shrews-dark       . "#002244")
    (sjy2-line-highlight    . "#E8EBF5")   ; current line bg
    (sjy2-modline-active    . "#96A6C8")
    (sjy2-modline-inactive  . "#D5D8D9")

    ;; Core Dark Accents 
    (sjy2-dark-bg           . "#181818")
    (sjy2-dark-fg           . "#FCFAF0")    ; #E4E4EF
    (sjy2-dark-fg-2         . "#A0A0A0")
    (sjy2-dark-bg-1         . "#282828")
    (sjy2-dark-bg-light     . "#2F3847")
    (sjy2-dark-bg-highlight . "#3C495C")
    (sjy2-niagara           . "#96a6c8")
    (sjy2-dark-bg-highlight-2 . "#252B35")
    (sjy2-dark-shrews       . "#A6B7DF")   ; cursor (dark)
    (sjy2-dark-modline-active . "#A6B7DF")
    (sjy2-dark-modline-inactive . "#35404F")))


(defconst sjy2-light-overrides
  `((background            ,(cdr (assoc 'sjy2-bg                sjy2-modus-palette)))
    (foreground            ,(cdr (assoc 'sjy2-fg                sjy2-modus-palette)))
    (bg-dim                ,(cdr (assoc 'sjy2-bg-1              sjy2-modus-palette)))
    (bg-alt                ,(cdr (assoc 'sjy2-bg-2          sjy2-modus-palette)))
    (bg-active             ,(cdr (assoc 'sjy2-bg-2          sjy2-modus-palette)))
    (bg-inactive           ,(cdr (assoc 'sjy2-bg-2          sjy2-modus-palette)))
    (comment               ,(cdr (assoc 'sjy2-quartz            sjy2-modus-palette)))
    (cursor                ,(cdr (assoc 'sjy2-shrews            sjy2-modus-palette)))
    (keyword               ,(cdr (assoc 'sjy2-wisteria          sjy2-modus-palette)))
    (builtin               ,(cdr (assoc 'sjy2-warning           sjy2-modus-palette)))
    (constant              ,(cdr (assoc 'sjy2-eyes              sjy2-modus-palette)))
    (err                   ,(cdr (assoc 'sjy2-error             sjy2-modus-palette)))
    (bg-mode-line-active   ,(cdr (assoc 'sjy2-modline-active    sjy2-modus-palette)))
    (bg-mode-line-inactive ,(cdr (assoc 'sjy2-modline-inactive  sjy2-modus-palette)))
    (bg-hl-line            ,(cdr (assoc 'sjy2-line-highlight    sjy2-modus-palette)))
    ;; Suggested additions for consistency
    ;; (string                ,(cdr (assoc 'sjy2-wisteria          sjy2-modus-palette)))
    ;; (docstring             ,(cdr (assoc 'sjy2-success           sjy2-modus-palette)))
    (fnname                ,(cdr (assoc 'sjy2-eyes              sjy2-modus-palette)))
    (variable              ,(cdr (assoc 'sjy2-fg-2              sjy2-modus-palette)))
    (fg-hl-line            ,(cdr (assoc 'sjy2-fg                sjy2-modus-palette)))
    (bg-region             ,(cdr (assoc 'sjy2-bg-2              sjy2-modus-palette)))
    (type                  unspecified)
    (border                unspecified)))


(defconst sjy2-dark-overrides
  `((background            ,(cdr (assoc 'sjy2-dark-bg             sjy2-modus-palette)))
    (foreground            ,(cdr (assoc 'sjy2-dark-fg             sjy2-modus-palette)))
    (bg-dim                ,(cdr (assoc 'sjy2-dark-bg-1           sjy2-modus-palette)))
    (bg-alt                ,(cdr (assoc 'sjy2-dark-bg-light       sjy2-modus-palette)))
    (bg-active             ,(cdr (assoc 'sjy2-dark-bg-highlight   sjy2-modus-palette)))
    (bg-inactive           ,(cdr (assoc 'sjy2-dark-bg-highlight-2 sjy2-modus-palette)))
    (comment               ,(cdr (assoc 'sjy2-quartz              sjy2-modus-palette)))
    (cursor                ,(cdr (assoc 'sjy2-dark-shrews         sjy2-modus-palette)))
    (keyword               ,(cdr (assoc 'sjy2-wisteria            sjy2-modus-palette)))
    (builtin               ,(cdr (assoc 'sjy2-warning             sjy2-modus-palette)))
    (constant              ,(cdr (assoc 'sjy2-eyes                sjy2-modus-palette)))
    (err                   ,(cdr (assoc 'sjy2-error               sjy2-modus-palette)))
    (bg-mode-line-active   ,(cdr (assoc 'sjy2-dark-modline-active sjy2-modus-palette)))
    (bg-mode-line-inactive ,(cdr (assoc 'sjy2-dark-modline-inactive sjy2-modus-palette)))
    (bg-hl-line            ,(cdr (assoc 'sjy2-dark-bg-highlight sjy2-modus-palette)))
    (fnname                ,(cdr (assoc 'sjy2-eyes              sjy2-modus-palette)))
    (variable              ,(cdr (assoc 'sjy2-dark-fg-2         sjy2-modus-palette)))
    (fg-hl-line            ,(cdr (assoc 'sjy2-dark-fg           sjy2-modus-palette)))
    (bg-region             unspecified)
    (fg-region             unspecified)
    (type                  unspecified)
    (border                unspecified)))


(use-package modus-themes
  :ensure t
  :custom
  ;; --- NEW CODE START ---
  (modus-themes-custom-faces
   ;; Use an alist of (FACE-NAME . PROPS)
   '(
     ;; Org Mode Heading Overrides for ALL Modus Themes
     (org-level-1 ((t (:foreground ,(cdr (assoc 'sjy2-shrews-dark sjy2-modus-palette))))))
     (org-level-2 ((t (:foreground ,(cdr (assoc 'sjy2-eyes        sjy2-modus-palette))))))
     (org-level-3 ((t (:foreground ,(cdr (assoc 'sjy2-wisteria    sjy2-modus-palette))))))
     (org-level-4 ((t (:foreground ,(cdr (assoc 'sjy2-niagara     sjy2-modus-palette))))))
     (org-level-5 ((t (:foreground ,(cdr (assoc 'sjy2-quartz      sjy2-modus-palette))))))
     ))
  ;; --- NEW CODE END ---
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs   t)
  (modus-themes-mixed-fonts       t)
  (modus-themes-headings '((1 . (1.4)) (2 . (1.3)) (3 . (1.2)) (4 . (1.1)) (t . (1.0))))
  (modus-themes-common-palette-overrides
   '((border-mode-line-active             bg-mode-line-active)
     (border-mode-line-inactive           bg-mode-line-inactive)))
  (modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted))
  ;; --- Apply Overrides ---
  (modus-operandi-palette-overrides        sjy2-light-overrides)
  (modus-operandi-tinted-palette-overrides sjy2-light-overrides)
  (modus-vivendi-palette-overrides         sjy2-dark-overrides)
  (modus-vivendi-tinted-palette-overrides  sjy2-dark-overrides)
  
  :config
  (load-theme 'modus-operandi-tinted t)
  (keymap-global-set "C-c w t" #'modus-themes-toggle)
  (add-hook 'modus-themes-after-load-theme-hook
            (lambda ()
              (when (featurep 'doom-modeline)
                (doom-modeline-refresh-bars)
                (force-mode-line-update t)))))


;; Fonts (centralised) ----------------------------------------------------
(defgroup sjy2-fonts nil
  "Font config for the sjy2 Emacs setup."
  :group 'faces)

(defcustom sjy2/default-font "Iosevka"
  "Default UI/monospace font family."
  :type 'string)

(defcustom sjy2/fixed-pitch-font "Victor Mono"
  "Font used for fixed-pitch buffers (code/term)."
  :type 'string)

(defcustom sjy2/variable-pitch-font "Iosevka"
  "Font used for variable-pitch/text buffers."
  :type 'string)

(defcustom sjy2/mode-line-font "JetBrains Mono"
  "Font used for the mode-line."
  :type 'string)

(defcustom sjy2/font-size 14
  "Base font size (pt)."
  :type 'integer)

;; Multilingual fonts with custom sizes
(defcustom sjy2/arabic-font "Noto Sans Arabic"
  "Preferred Arabic script font."
  :type 'string)

(defcustom sjy2/arabic-size 30
  "Font size for Arabic script."
  :type 'integer)

(defcustom sjy2/hebrew-font "Noto Sans Hebrew"
  "Preferred Hebrew script font."
  :type 'string)

(defcustom sjy2/hebrew-size 28
  "Font size for Hebrew script."
  :type 'integer)

(defcustom sjy2/devanagari-font "Noto Sans Devanagari"
  "Preferred Devanagari font."
  :type 'string)

(defcustom sjy2/devanagari-size 30
  "Font size for Devanagari script."
  :type 'integer)

(defcustom sjy2/greek-font "Noto Serif"
  "Preferred font for polytonic Greek."
  :type 'string)

(defcustom sjy2/greek-size 26
  "Font size for polytonic Greek."
  :type 'integer)

;; Set default Latin font FIRST
(set-face-attribute 'default nil
                    :family sjy2/default-font
                    :height (* sjy2/font-size 10)) ; height is in 1/10pt units

(defun sjy2/apply-multilingual-fonts ()
  "Apply custom fonts and sizes for specific non-Latin scripts."
  ;; Arabic
  (set-fontset-font t 'arabic
                    (font-spec :family sjy2/arabic-font :size sjy2/arabic-size)
                    nil 'prepend)
  ;; Hebrew
  (set-fontset-font t 'hebrew
                    (font-spec :family sjy2/hebrew-font :size sjy2/hebrew-size)
                    nil 'prepend)
  ;; Devanagari
  (set-fontset-font t 'devanagari
                    (font-spec :family sjy2/devanagari-font :size sjy2/devanagari-size)
                    nil 'prepend)
  ;; Greek (including polytonic)
  (set-fontset-font t 'greek
                    (font-spec :family sjy2/greek-font :size sjy2/greek-size)
                    nil 'prepend))

;; Apply on startup
(sjy2/apply-multilingual-fonts)


;; VTerm per-buffer faces -------------------------------------------------
(defun sjy2/vterm-buffer-appearance ()
  "Apply vterm-specific face tweaks to the current buffer only."
  (when (derived-mode-p 'vterm-mode)
    ;; buffer-local face background/foreground via `buffer-face-mode'
    (setq buffer-face-mode-face `(:background "#574F4A" :foreground "#95A99F"))
    (buffer-face-mode 1)
    ;; fringe and cursor adjustments (global faces changed only locally where safe)
    (set-face-attribute 'fringe nil :background "#574F4A")
    (set-face-attribute 'cursor nil :background "white")
    ;; hl-line disabled for vterm to avoid clash
    (when (boundp 'hl-line-mode)
      (hl-line-mode -1))))
(add-hook 'vterm-mode-hook #'sjy2/vterm-buffer-appearance)

;; Only run icon setup if fonts are present. This avoids warnings on headless installs.
(use-package nerd-icons
  :defer t
  :custom (nerd-icons-font-family "Symbols Nerd Font Mono")
  :config
  (when (member "Symbols Nerd Font Mono" (font-family-list))
    ;; Prepend to unicode fontset so icons display robustly
    (set-fontset-font t 'unicode "Symbols Nerd Font Mono" nil 'prepend)))

(use-package all-the-icons
  :defer t
  :custom (all-the-icons-scale-factor 1.0)
  :config
  (when (member "Symbols Nerd Font Mono" (font-family-list))
    (set-fontset-font t 'unicode "Symbols Nerd Font Mono" nil 'prepend)))

;;;; Mixed-pitch (for prose) -------------------------------------------------
(use-package mixed-pitch
  :defer nil
  :hook ((text-mode . mixed-pitch-mode)
         (org-mode . mixed-pitch-mode)
         (markdown-mode . mixed-pitch-mode)
         (latex-mode . mixed-pitch-mode)) ; Add latex-mode here
  :config
  (setq mixed-pitch-face 'variable-pitch))


;; Variable-pitch in prose modes 
(dolist (hook '(text-mode-hook org-mode-hook markdown-mode-hook latex-mode-hook))
  (add-hook hook #'variable-pitch-mode))


(custom-set-faces
 '(fixed-pitch            ((t (:family "Iosevka"))))
 '(variable-pitch         ((t (:family "Iosevka"))))
 '(font-lock-comment-face ((t (:slant italic))))
 '(font-lock-comment-face ((t (:foreground "#95A99F" :slant italic))))
 '(term                   ((t (:background "#002244" 
					   :foreground "#95A99F" :weight bold))))
 '(keycast-key            ((t (:background "#6D7587" 
					   :foreground "#E4C869" :weight bold)))))

;;; ———————————————————————— 06 Dired ————————————————————————
(use-package dired-filter
  ;; https://github.com/Fuco1/dired-hacks?tab=readme-ov-file#packages
  :ensure t
  :hook (dired-mode . dired-filter-mode))

(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-Alhv --group-directories-first --time-style=long-iso")
  (dired-dwim-target t)                    ; smart two-pane copying/moving
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)           ; ask once for recursive delete
  (dired-auto-revert-buffer t)
  (dired-hide-details-mode t)              ; cleaner by default
  (dired-omit-files "^\\...*\\'")          ; hide dotfiles + ".." line
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-create-destination-dirs 'ask)
  (dired-mouse-drag-files t)
  (dired-guess-shell-alist-user
   '(("\\.\\(pdf\\|djvu\\)\\'" "evince")
     ("\\.\\(mp4\\|mkv\\|webm\\)\\'" "mpv")
     ("\\.html?\\'" "firefox")))
  :hook
  (dired-mode . dired-omit-mode)
  (dired-mode . hl-line-mode)
  (dired-mode . dired-filter-mode)          ; orderless/vertico sorting
  :bind
  (("s-d" . dired)
   ("s-D" . dired-other-window)
   :map dired-mode-map
   ("RET"   . dired-find-alternate-file)     ; reuse buffer (classic)
   ("^"     . dired-up-directory)
   ("b"     . dired-up-directory)            ; your preferred binding
   ("h"     . dired-omit-mode)               ; toggle hidden files
   ("e"     . dired-create-empty-file)
   ("C-+"   . dired-create-directory)
   ("I"     . dired-image-show-this-file)    ; Emacs 30+ built-in preview
   ("C-c C-q" . wdired-change-to-wdired-mode)))

;; WDired – editable dired (built-in, just enable the good defaults)
(use-package wdired
  :ensure nil
  :after dired
  :custom
  (wdired-allow-to-change-permissions t)
  (wdired-create-parent-directories t))

;; Icons – only one package (nerd-icons-dired > all-the-icons-dired in 2025)
(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

;; Subtree expansion – the one everyone actually uses in 2025
(use-package dired-subtree
  :ensure t
  :after dired
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)      ; simple & obvious
              ("<backtab>" . dired-subtree-remove)
              ("i" . dired-subtree-insert))
  :custom
  (dired-subtree-line-prefix "    ")
  (dired-subtree-use-backgrounds nil))

;; Git info in the margin – tiny & perfect
(use-package dired-git-info
  :ensure t
  :bind (:map dired-mode-map
              (")" . dired-git-info-mode)))

;; Optional: fd-dired – super fast search → dired buffer (highly recommended)
(use-package fd-dired
  :ensure t
  :bind ("C-c f d" . fd-dired))


;;; ———————————————————————— 07 SIMPLE Packages ————————————————————————
(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line)
         ;; ("C-k" . crux-smart-kill-line)
         ("C-^" . crux-top-join-line)))

(use-package multiple-cursors
  ;; :vc (:url "https://github.com/magnars/multiple-cursors.el" :rev :newest)
  :demand t
  :config
  (keymap-global-set "C-M-l"       'mc/mark-all-like-this)
  (keymap-global-set "C-M-/"       'mc/mark-all-in-region)
  (keymap-global-set "C-M-j"       'mc/mark-next-like-this)
  (keymap-global-set "M-S-<down>"  'mc/mark-next-like-this)
  (keymap-global-set "C-M-k"       'mc/mark-previous-like-this)
  (keymap-global-set "M-S-<up>"    'mc/mark-previous-like-this)
  :bind
  (:map org-mode-map
        :package org
        ("M-S-<up>"  . #'mc/mark-previous-like-this)
        ("M-S-<down>". #'mc/mark-next-like-this)))


;; 1. Tree-sitter
(use-package treesit-auto
  :ensure t
  :defer 2  ; Wait for Emacs to fully initialize
  :custom
  (treesit-auto-install 'always)
  :init
  ;; Configure grammar sources BEFORE treesit-auto loads
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (yaml "https://github.com/tree-sitter/tree-sitter-yaml")
          (delete 'markdown treesit-auto-lang-recipe-alist)
          (delete 'markdown-inline treesit-auto-lang-recipe-alist)
	  :config
	  (global-treesit-auto-mode))))



;; 2. Eglot: Only enable for languages where LSP servers are installed
(use-package eglot
  :ensure t
  :custom
  (eglot-autoshutdown t)  ; Kill server when last buffer closes
  (eglot-sync-connect 0)  ; Don't block on slow servers
  
  :config
  ;; Only hook modes where you've installed LSP servers
  (add-hook 'python-mode-hook #'eglot-ensure)    ; if pyright/pylsp installed
  (add-hook 'js-mode-hook #'eglot-ensure)        ; if typescript-language-server
  (add-hook 'rust-mode-hook #'eglot-ensure)      ; if rust-analyzer
  ;; Suppress warnings for missing servers
  (defun sjy2-eglot-ensure-safe ()
    "Start Eglot only if LSP server is available."
    (when (and (eglot--lookup-mode major-mode)
               (eglot--guess-contact))
      (eglot-ensure))))

;; THE git porcelain
(use-package magit)

(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :init
  ;; Fix compatibility issue with revert-buffer-in-progress
  (unless (boundp 'revert-buffer-in-progress)
    (defvar revert-buffer-in-progress nil))
  :config
  (diff-hl-flydiff-mode 1)
  (unless (display-graphic-p) 
    (diff-hl-margin-mode 1)))


;; Run Emacs Lisp functions asynchronously in separate Emacs processes
(use-package async
  ;; :vc (:url "https://github.com/jwiegley/emacs-async.git")
  :config
  (dired-async-mode 1)
  (async-bytecomp-package-mode 1)) 

;; An Emacs package to move point through buffer-undo-list positions.
(use-package goto-last-change
  ;; :vc (:url "https://github.com/camdez/goto-last-change.el" :rev :newest)
  :bind ("C-c g" . goto-last-change))

;; A minor mode which displays current (and total) matches in the mode-line.
(use-package anzu
  ;; :vc (:url "https://github.com/emacsorphanage/anzu" :rev :newest)
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp))
  :config 
  (global-anzu-mode 1))

;; ;; native way of showing the counter without anzu package
;; (setopt isearch-lazy-count t)
;; (setopt lazy-count-prefix-format "(%s/%s) ")
;; (setopt lazy-count-suffix-format nil)

;; Mare regular isearch interpret the empty space as a rexex
;; that matches any character between the words you give it
(setopt search-whitespace-regexp ".*")

;; A package for jumping to visible text using a char-based decision tree
(use-package avy
  ;; :vc (:url "https://github.com/abo-abo/avy" :rev :newest)
  :config
  (setopt avy-timeout-seconds 0.20)
  :bind (("C-c j"   . #'avy-goto-char-timer)     
         ;; ("M-j"     . #'avy-goto-char-timer)     ; already set to C-j. M-j --- insert newline etc.
         ("M-s-y"   . #'avy-copy-line)
         ))

;; Expand region increases the selected region by semantic units.
(use-package expand-region
  ;; :vc (:url "https://github.com/magnars/expand-region.el" :rev :newest)
  :config
  (keymap-global-set "M-=" #'er/expand-region)
  (keymap-global-set "M--" #'er/contract-region))


(use-package smartparens
  :ensure t
  :demand t
  :hook (prog-mode . smartparens-strict-mode)
  :bind (:map smartparens-mode-map
              ;; Navigation
              ("C-M-f" . sp-forward-sexp)
              ("C-M-b" . sp-backward-sexp)
              ("C-M-n" . sp-next-sexp)
              ("C-M-p" . sp-previous-sexp)
              
              ;; Manipulation
              ("C-M-k" . sp-kill-sexp)
              ("C-M-w" . sp-copy-sexp)
              ("C-M-t" . sp-transpose-sexp)
              
              ;; Slurp/barf (expand/contract)
              ("C-)" . sp-forward-slurp-sexp)
              ("C-}" . sp-forward-barf-sexp)
              ("C-(" . sp-backward-slurp-sexp)
              ("C-{" . sp-backward-barf-sexp)
              
              ;; Wrapping (region must be active)
              ("M-(" . sp-wrap-round)
              ("M-[" . sp-wrap-square)
              ("M-{" . sp-wrap-curly)
              
              ;; Unwrap/rewrap
              ("M-<delete>" . sp-unwrap-sexp)
              ("M-<backspace>" . sp-backward-unwrap-sexp))

  
  :config
  (require 'smartparens-config)
  
  ;; Enable globally
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  
  ;; Don't be too smart in minibuffer
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  
  ;; Lisp-specific: don't pair ' (it's a quote!)
  (sp-with-modes sp-lisp-modes
    (sp-local-pair "'" nil :actions nil)))

;; If you want a shorter binding:
(with-eval-after-load 'org
  (keymap-set org-mode-map "C-c e" #'org-emphasize))

;;; Quick Reference

;; Wrapping (select region first):
;;   M-(  → wrap with ()
;;   M-[  → wrap with []
;;   M-{  → wrap with {}

;; Navigation:
;;   C-M-f/b  → forward/backward sexp
;;   C-M-n/p  → next/previous sexp

;; Slurping (expand parentheses):
;;   C-)  → slurp forward    (a b) |c   →  (a b c)|
;;   C-(  → slurp backward   a |(b c)   →  |(a b c)

;; Barfing (contract parentheses):
;;   C-}  → barf forward     (a b c)|   →  (a b)| c
;;   C-{  → barf backward    |(a b c)   →  a |(b c)

;; Unwrapping:
;;   M-<delete>      → unwrap forward
;;   M-<backspace>   → unwrap backward

;; Org emphasis (with C-c e binding):
;;   Select text → C-c e → b (bold)
;;   Select text → C-c e → i (italic)
;;   Select text → C-c e → c (code)
;;   Select text → C-c e → v (verbatim)


;; Alter one occurrence of something, and change the others. Visually.
(use-package iedit
  ;; :vc (:url "https://github.com/victorhge/iedit" :rev :newest)
  :config
  (keymap-global-set "C-c i" #'iedit-mode))

;; A Collection of Ridiculously Useful eXtensions for Emacs.
(use-package crux  
  ;; :vc (:url " https://github.com/bbatsov/crux" :rew :newest)
  )

;; Deadgrep: Fast, beautiful project text search
(use-package deadgrep
  ;; :vc (:url "https://github.com/Wilfred/deadgrep" :rev :newest)
  :bind ("<f5>" . deadgrep))

;; ripgrep wrapper with Emacs integration
(use-package rg
  ;; :vc (:url "https://github.com/dajva/rg.el" :rev :newest)
  :config
  (rg-enable-default-bindings))

;; Writable grep buffers
(use-package wgrep
  ;; :vc (:url "https://github.com/mhayashi1120/Emacs-wgrep")
  :commands (wgrep wgrep-change-to-wgrep-mode))

;; Visual regexp with live feedback
(use-package visual-regexp)

;; Adds modern regexp engine support to visual-regexp
(use-package visual-regexp-steroids
  ;; :vc (:url "https://github.com/benma/visual-regexp-steroids.el")
  :after visual-regexp
  :bind (("C-c r" . vr/replace)
         ("C-c q" . vr/query-replace)
         ("C-c m" . vr/mark))
  :config
  ;; These bindings go in esc-map (aka M- prefix), not global map
  (define-key esc-map (kbd "C-M-r") #'vr/isearch-backward)
  (define-key esc-map (kbd "C-M-s") #'vr/isearch-forward))

;; a minor mode that makes it possible to drag stuff (words, region, lines) around. 
(use-package drag-stuff
  ;; :vc (:url "https://github.com/rejeep/drag-stuff.el")
  :ensure t
  :demand t
  :bind(("M-<UP>"    . drag-stuff-up)
        ("M-<DOWN>"  . drag-stuff-down))
  :config
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))


;; ———————————————————————— Beacon – fixed, no more post-command-hook error ————————————————————————
;; (use-package beacon
;;   :ensure t
;;   :hook (after-init . beacon-mode)
;;   :custom
;;   (beacon-push-mark t)
;;   (beacon-size 50)
;;   (beacon-blink-when-focused t)
;;   (beacon-blink-when-buffer-changes t)
;;   (beacon-blink-when-window-scrolls t)
;;   (beacon-blink-when-window-changes t)
;;   (beacon-blink-duration 0.25)
;;   (beacon-blink-delay 0.1)
;;   (beacon-color "#F8BB7C"))

;; Never lose your cursor
(use-package beacon
  ;; :vc (:url "https://github.com/Malabarba/beacon" :rev :newest)
  :hook (after-init . beacon-mode)
  :custom
  (beacon-color "#F8BB7C"))

;; Pulse highlight current line on switch window
(use-package pulsar
  :config
  (setopt pulsar-pulse 1)
  (setopt pulsar-delay 0.055)
  (setopt pulsar-iterations 10)
  (setopt pulsar-face 'pulsar-magenta)
  (setopt pulsar-highlight-face 'pulsar-yellow)
  (pulsar-global-mode 1))



;; Colorize color hex, RGB etc, names in buffers
;; https://github.com/emacsmirror/rainxbow-mode

;; More complete than rainbow mode?
(use-package colorful-mode
  ;; :vc (:url "https://github.com/DevelopmentCool2449/colorful-mode" :rev :newest)
  :custom
  (colorful-use-prefix t)
  (colorful-only-strings t) ;'only-prog)
  (css-fontify-colors t)
  (colorful-color-face-attributes
   '(:background :foreground))  
  :config
  (global-colorful-mode 1)
  (add-to-list 'global-colorful-modes 'helpful-mode))

;; ;; Pretty hex colours
;; (use-package rainbow-mode
;;   ;; :vc (:url "https://github.com/emacsmirror/rainbow-mode/" :rev :newest)
;;   :hook (after-init . rainbow-mode)
;;   :config
;;   (rainbow-mode 1))

;; Functions providing the inverse of Emacs' fill-paragraph and fill-region
;; https://github.com/purcell/unfill
;; M-x unfill-region
;; M-x unfill-paragraph
;; M-x unfill-toggle
(use-package unfill
  :config
  (keymap-global-set "C-c M-q" #'unfill-paragraph)
  (keymap-global-set "C-x M-q" #'unfill-toggle))

;; Simple, linear, light-weight wrapper for Emacs built-in undo system.
(use-package undo-fu
  :config
  (keymap-global-set "C-z"   #'undo-fu-only-undo)
  (keymap-global-set "C-S-z" #'undo-fu-only-redo))


;; Displays the undo history as a navigable tree of previous buffer states.
(use-package vundo
  :config
  (setopt vundo-glyph-alist vundo-unicode-symbols)
  (setopt vundo-compact-display t)
  (keymap-global-set "C-M-u" #'vundo))

;; very large files... loads them in chunks
(use-package vlf)


;; Unparalleled performance and compatibility with standard command-line tools.
(use-package vterm
  :config
  (setopt vterm-max-scrollback 100000)
  (setopt vterm-shell "bash")
  (add-hook 'vterm-mode-hook 'sjy2/buffer-face-mode)
  )

;; Toggle between vterm buffer and whatever buffer you are editing.
(use-package vterm-toggle
  :ensure t
  :bind (("C-c v t" . vterm-toggle)  ; Fixed: full kbd string (was truncated)
         :map vterm-mode-map
         ("C-`" . vterm-toggle-hide))  ; hide from vterm
  :custom
  (vterm-toggle-scope 'workspace)  ; per-workspace terminals
  (vterm-toggle-fullscreen-p nil)) ; no fullscreen by default



;; smoother scrolling
(use-package ultra-scroll
  ;; :vc (:url "https://github.com/jdtsmith/ultra-scroll" :rev :newest)
  :config
  (setopt scroll-conservatively 101 ; important!
          scroll-margin 0) 
  (ultra-scroll-mode 1))



;;; ———————————————————————— 065 formats / langs ————————————————————————

;; Markdown
;; https://jblevins.org/projects/markdown-mode/
(use-package markdown-mode
  ;; :vc (:url "https://jblevins.org/projects/markdown-mode/" :rev :newest)
  :mode 
  (("README\\.md\\.markdown\\'" . gfm-mode)) ;; github flavour
  :config 
  (setopt markdown-command "pandoc")
  ;;(setopt markdown-command "multimarkdown"))
  (setopt markdown-fontify-code-blocks-natively t)
  (add-hook 'markdown-mode-hook
            (lambda ()
              (when buffer-file-name
                (add-hook 'after-save-hook 'check-parens nil t))))
  ;; TS remap for full TS integration
  (add-to-list 'major-mode-remap-alist '(gfm-mode . gfm-ts-mode)))


;;; CSV
(use-package csv-mode
  :mode ("\\.[Cc][Ss][Vv]\\'" . python-mode)
  :config
  (setopt csv-separators '("," ";" "|" " " ", " "\t")))

;;; YAML
(use-package yaml-mode)

;;; ———————————————————————— 07 Completions & Minor QoL ————————————————————————
;; Vertico + Orderless + Marginalia = the only completion stack anyone uses in 2025
(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-resize t)
  (vertico-count 15)
  :config
  ;; tiny arrow for current candidate (optional but cute)
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
                (setq cand (funcall orig cand prefix suffix index _start))
                (concat (if (= vertico--index index)
                            (propertize " » " 'face 'vertico-current)
                          "   ")
                        cand))))



(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode)
  :custom
  ;; Core vertico settings
  (vertico-cycle t)
  (vertico-count 15)
  (vertico-resize t)
  (vertico-scroll-margin 4)
  (completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (enable-recursive-minibuffers t)
  ;; Protect minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  :config
  ;; Activate cursor-intangible-mode for protected prompt
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  ;; Visual indicator for current candidate
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
                (setq cand (funcall orig cand prefix suffix index _start))
                (concat
                 (if (= vertico--index index)
                     (propertize "»» " 'face 'vertico-current)
                   "   ")
                 cand))))



;; (use-package orderless
;;   :ensure t
;;   :custom
;;   (completion-styles '(orderless basic))
;;   (completion-category-overrides '((file (styles basic partial-completion))))
;;   (orderless-matching-styles '(orderless-flex orderless-regexp)))


;; (use-package orderless
;;   ;; :vc (:url "https://github.com/oantolin/orderless" :rev :newest)
;;   :after (vertico)
;;   :custom
;;   (orderless-matching-styles 'orderless-regexp)
;;   ;;TODO: what is this? ( orderless-component-separator #'orderless-escapable-split-on-space)
;;   (completion-styles '(orderless flex basic))
;;   ;; flex-style matching (eg.  bk matches book) for C-x C-f
;;   ;; initials matching (ttl matches toggle-truncate-lines) for M-x.
;;   (completion-category-defaults nil)
;;   (completion-category-overrides '((file (styles basic partial-completion)))))


(use-package orderless
  :ensure t
  :custom
  (orderless-matching-styles 
   '(orderless-literal orderless-regexp orderless-initialism))
  (completion-category-defaults nil)
  (completion-category-overrides 
   '((file (styles basic partial-completion))
     (eglot (styles basic))))
  (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless flex basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode)
  :custom (marginalia-align 'right))

;; Helpful – the only help replacement worth having
(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h o" . helpful-symbol)
         ("C-h ." . helpful-at-point)))

;; ace-window – still the best window switcher (M-o is perfect)
(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-dispatch-always t)
  :config
  (ace-window-display-mode 1)
  ;; keep only the actions you actually use
  (setq aw-dispatch-alist
        '((?x aw-delete-window     "Delete")
          (?m aw-swap-window       "Swap")
          (?F aw-split-window-fair "Fair Split")
          (?v aw-split-window-vert "Split Vert")
          (?h aw-split-window-horz "Split Horz")
          (?o delete-other-windows "Maximize")
          (?? aw-show-dispatch-help))))

;; rainbow-delimiters – still unbeatable in lisp modes
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; zzz-to-char – M-z with avy power (still better than built-in zap-up-to-char)
(use-package zzz-to-char
  :ensure t
  :bind ("M-z" . zzz-to-char))

;; all-the-icons-completion – tiny polish, zero cost
(use-package all-the-icons-completion
  :ensure t
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :config (all-the-icons-completion-mode))

;; aggressive-indent – keep it only where it actually helps (Emacs Lisp)
(use-package aggressive-indent
  :ensure t
  :hook (emacs-lisp-mode . aggressive-indent-mode))

;; Log all Emacs interactions to a buffer (local package)
(use-package interaction-log
  :load-path "~/.config/emacs/git-cloned-lisp/"
  :demand t  ; Load immediately since you want it always active
  :bind ("C-h C-l" . interaction-log-display-buffer)
  :config
  (interaction-log-mode +1)
  (defun interaction-log-display-buffer ()
    "Display the interaction log buffer."
    (interactive)
    (display-buffer ilog-buffer-name)))


;;; ———————————————————————— 08 Completions ————————————————————————
(use-package tempel
  :demand t
  :custom
  (tempel-trigger-prefix "<")      ; Type "<" to trigger template menu
  :bind (:map sjy2/prefix-map
              ("t c" . tempel-complete) ; M-m t c
              ("t i" . tempel-insert)   ; M-m t i
              ("t e" . tempel-expand)   ; Expand template
              ("t n" . sjy2/tempel-expand-or-next))
  :config
  (setopt tempel-path (expand-file-name "tempel-snippets.el" sjy2-etc-dir))
  
  (defun sjy2/tempel-expand-or-next ()
    "If in a template field, go to next field; otherwise expand template."
    (interactive)
    (if tempel--active
        (tempel-next 1)
      (tempel-expand t))))

;; Dabbrev
(setq-default abbrev-mode t) ; Turn on abbrev mode by default
(setopt abbrev-file-name (expand-file-name "abbrev_defs" sjy2-etc-dir))

(abbrev-table-put global-abbrev-table :regexp "\\(?:^\\|[\t\s]+\\)\\(?1:[:_].*\\|.*\\)")

(add-hook 'after-init-hook
          (lambda ()
            (read-abbrev-file))) ; Reads abbrev_defs and loads the 'global-abbrev-table'

(keymap-global-set "M-/" #'dabbrev-expand)

;; Fucking hippies
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially        ; File names first (handy!)
        try-expand-dabbrev                      ; Words in current buffer
        try-expand-dabbrev-all-buffers          ; Words in all buffers
        try-expand-dabbrev-from-kill            ; Words from kill ring
        try-complete-lisp-symbol-partially))    ; Lisp symbols last
(keymap-global-set "C-M-/" #'hippie-expand) ; Alternative: C-M-/

;;; ———————————————————————— 09 Consult, Embark etc ————————————————————————

;; Consult – the one true replacement for almost everything
(defun sjy2/consult-line-all-buffers ()
  "Always search ALL buffers with consult-line-multi."
  (interactive)
  (let ((current-prefix-arg '(4)))  ; Simulate C-u
    (call-interactively #'consult-line-multi)))

(use-package consult
  :ensure t
  :demand t
  :bind
  (;; C-x replacements
   ("C-x b"   . consult-buffer)
   ("C-x B"   . consult-buffer-other-window)
   ("C-x C-f" . consult-buffer-other-frame)
   ("C-x C-r" . consult-recent-file)
   ("C-x r b" . consult-bookmark)
   ("C-x p b" . consult-project-buffer)
   ;; M-g replacements
   ("M-g g"   . consult-goto-line)
   ("M-g M-g" . consult-goto-line)
   ("M-g o"   . consult-outline)
   ("M-g i"   . consult-imenu)
   ("M-g I"   . consult-imenu-multi)
   ;; M-s searching (project limited;)
   ("M-s o" . consult-line-multi)            ; Search poject buffers 
   ("M-s O" . sjy2/consult-line-all-buffers) ; Search all open buffers
   ("M-s g" . consult-grep)                  ; Grep project directory
   ("M-s G" . consult-git-grep)              ; Git grep (if in repo)
   ("M-s r" . consult-ripgrep)               ; Ripgrep (fastest)
   ("M-s f" . consult-find)                  ; Find files
   ("M-s l" . consult-line)                  ; Search current buffer
   ("M-s i" . consult-imenu)                 ; Jump to definitions
   ("M-s m" . consult-mark))                 ; Jump to marks
  ;; registers
  ("M-'"     . consult-register-store)
  ("M-#"     . consult-register-load)
  :custom
  (xref-show-xrefs-function        #'consult-xref)
  (xref-show-definitions-function  #'consult-xref)
  ;; For external projects, install prot-consult: M-x package-install RET prot-consult
  (consult-project-root-function   #'consult-project-root-function)  ; safe default
  ;; FD-powered ripgrep (replaces consult-fd)
  (consult-ripgrep-args
   "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --with-filename --line-number --search-zip --hidden --glob=!.git/"))

;; recent directories in minibuffer (tiny quality-of-life)
(use-package consult-dir
  :ensure t
  :bind (:map minibuffer-local-map
              ("C-x C-d" . consult-dir)))

;; Embark – “right-click for Emacs” (you said you want to learn it → keep minimal & useful)
(use-package embark
  :ensure t
  :bind (("C-."   . embark-act)         ; the single most useful key
         ("C-;"   . embark-dwim)        ; alternative “do the right thing”
         ("C-h B" . embark-bindings)) ; like C-h b but for current context

  :custom
  (prefix-help-command #'embark-prefix-help-command) ; makes C-h after prefix useful
  :config
  ;; tiny which-key integration – shows actions when you wait a moment
  (setq embark-indicators
        '(embark-minimal-indicator  ; tiny “…” in echo area
          embark-highlight-indicator
          embark-isearch-highlight-indicator)))

;; embark-consult – makes embark work beautifully with consult buffers
(use-package embark-consult
  :ensure t
  :after (embark consult))

(use-package which-key
  :ensure t
  :demand t
  :diminish which-key-mode
  :custom (which-key-idle-delay 0.8)
  :config (which-key-mode 1))

;; Corfu – the modern, non-intrusive in-buffer completion 
(use-package corfu
  :ensure t
  :hook ((prog-mode) . corfu-mode) ; *scratch* is a prog mode?
  :custom
  (corfu-auto t)           ; only pops up when you actually want it
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.1)
  (corfu-cycle t)
  (corfu-quit-no-match t)
  (corfu-preselect 'prompt)
  :bind (:map corfu-map
              ("TAB"     . corfu-next)
              ("<backtab>" . corfu-previous)
              ("RET"     . corfu-insert))
  :config
  ;; (global-corfu-mode 1)
  (corfu-history-mode 1)
  (corfu-popupinfo-mode 1)) ; shows docs on right side



;;; ———————————————————————— 10 Latex and Bibtex ————————————————————————
;; sudo apt install bibtool
;; bibtool --preserve.key.case=on --print.deleted.entries=off -s -d ~/documents/library/mendeley_export.bib ~/Documents/library/'Exported Items.bib' -o Mendeley-combined.bib


;; 2025 idiomatic version – lazy, clean, safe
(use-package sjy2-latex
  :load-path "sjy2-lisp"                ; or "~/.config/emacs/sjy2-lisp/"
  :defer t                              ; ← don’t load until actually needed
  :commands (sjy2/latex-setup sjy2/auctex-extra)
  :mode ("\\.tex\\'" . LaTeX-mode)      ; or whatever triggers it
  :hook (LaTeX-mode . sjy2-latex-options)  ; if it just adds a hook
  :init
  (setq sjy2-latex-options t)           ; set variable early if needed
  :config
  (message "sjy2-latex loaded from %s" (locate-library "sjy2-latex")))

;; Custom LaTeX to DOCX/RTF conversion function
(defun sjy2/latex-to-docx (output-format)
  "Convert LaTeX file to DOCX or RTF using Pandoc."
  (interactive "sEnter output format (docx/rtf): ")
  (let ((output-file (concat (file-name-sans-extension buffer-file-name) "." output-format)))
    (shell-command (format "pandoc %s -o %s" (buffer-file-name) output-file))
    (message "Converted to %s format." output-file)))

;; Ensure latex-mode-map is available before defining the keybinding
(add-hook 'latex-mode-hook
          (lambda ()
            (define-key latex-mode-map (kbd "C-c C-d") 'sjy2/latex-to-docx)))

;; Doc-view
(use-package doc-view
  :ensure nil ; built-in
  :custom
  (doc-view-resolution 300)
  (large-file-warning-threshold (* 50 (expt 2 20))))

;; Read ePub files
(use-package nov
  ;; :vc (:url "https://github.com/emacsmirror/nov" :rev :newest)
  :ensure t
  :custom
  (nov-save-headers-in-buffer t)
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (defvar nov-save-dir (expand-file-name "nov-saves/" (or (boundp 'sjy2-etc-dir) user-emacs-directory)))
  (make-directory nov-save-dir t))


(use-package sjy2-bibtex
  :load-path "sjy2-lisp/"       ; absolute path works perfectly
  :defer t
  :hook (bibtex-mode . sjy2-bibtex-setup))   ; whatever function it provides



;; Browse and gather bibliographic references and publications from various sources, by keywords or by DOI
(use-package biblio)

;;; ———————————————————————— 11 Notetaking & Writing (final 2025 perfection) ————————————————————————

;; 1. Jinx – on-demand spell-checking (your preferred bindings + prefix map)
(use-package jinx
  :ensure t
  :diminish
  :hook (text-mode . jinx-mode)
  :bind (("M-$"     . jinx-correct)
         ("C-M-$"   . jinx-languages)
         ("C-c s"   . my-spelling-map))
  :custom
  (jinx-languages "en_GB en_US es")
  (jinx-extra-dictionaries
   (list
    (expand-file-name "sjy2-jinx-custom-dict.txt" sjy2-etc-dir)))
  :config
  (define-prefix-command 'my-spelling-map)
  (keymap-set my-spelling-map "c" #'jinx-correct)
  (keymap-set my-spelling-map "n" #'jinx-next)
  (keymap-set my-spelling-map "p" #'jinx-previous)
  (keymap-set my-spelling-map "l" #'jinx-languages)
  (keymap-set my-spelling-map "a" #'jinx-correct-all)
  (keymap-set my-spelling-map "w" #'jinx-correct-word)
  (keymap-set my-spelling-map "N" #'jinx-correct-nearest))

;; 2. Olivetti – minimalist writing experience
(use-package olivetti
  :ensure t
  :bind ("C-c o" . olivetti-mode)
  :custom
  (olivetti-body-width 125)
  (olivetti-minimum-body-width 100)
  (olivetti-recall-visual-line-mode-entry-state t)
  :hook (olivetti-mode . (lambda ()
                           (when olivetti-mode
                             (display-line-numbers-mode -1)))))

;; 3. Denote – simple notetaking
(use-package denote
  :ensure t
  :demand t
  :custom
  (denote-directory "~/MEGA/emacs-notes/denote/")
  ;;(setopt denote-directory (expand-file-name "denote/"     sjy2-etc-dir))

  (denote-known-keywords '("configx" "foodx" "govx" "jobx" "jokex" "langx" "notex" "maranathax"
                           "mediax" "persx" "pornx" "quotex" "readx" "refx" "religx"
                           "techx" "whoknewx" "wordx"))
  (denote-infer-keywords t)
  (denote-sort-keywords t)
  (denote-file-type 'org)
  (denote-prompts '(title keywords))
  (denote-date-prompt-use-org-read-date t)
  (denote-date-format "%Y-%m-%dT%H:%M(%a)")
  :bind (("C-c n n"   . denote)
         ("C-c n N"   . denote-type)
         ("C-c n d"   . denote-date)
         ("C-c n z"   . denote-signature)
         ("C-c n i"   . denote-link-or-create)
         ("C-c n l"   . denote-find-link)
         ("C-c n b"   . denote-find-backlink)
         ("C-c n f"   . sjy2/consult-denote-search)  ; filename search
         ("C-c n g"   . sjy2/consult-denote-grep)) 
  :hook ((dired-mode  . denote-dired-mode)
         (text-mode   . denote-fontify-links-mode-maybe))
  :config
  (denote-rename-buffer-mode 1)
  (set-register ?d `(file . ,denote-directory))
  (set-register ?n `(file . "~/Documents/notes/"))
  (set-register ?s `(file . "~/_scratch/"))
  (setq denote-templates
        '((default . "#+title: %s\n#+date: %s\n#+filetags: %s\n\n* ")))

  ;; Enhanced search with better preview and sorting
  (defun sjy2/consult-denote-search ()
    "Search Denote notes by file name with preview."
    (interactive)
    (find-file
     (consult--read
      (denote-directory-files)
      :prompt "Find Note: "
      :sort t
      :require-match t
      :category 'file
      :lookup #'consult--lookup-member
      :state (consult--file-preview)
      :annotate (lambda (cand)
                  (when-let* ((attrs (file-attributes cand)))
                    (format " %s"
                            (format-time-string "%Y-%m-%d %H:%M"
                                                (file-attribute-modification-time attrs))))))))
  
  ;; Simplified grep (use consult-ripgrep's native directory support)
  (defun sjy2/consult-denote-grep ()
    "Ripgrep inside Denote notes."
    (interactive)
    (consult-ripgrep denote-directory)))

;; 4. One-key writing session (enhanced with auto-save)
(defun sjy2/writing-session ()
  "Start a focused Denote writing session with auto-save."
  (interactive)
  (call-interactively #'denote)
  (olivetti-mode 1)
  (jinx-mode 1)
  (auto-save-visited-mode 1)  ; Auto-save while writing
  (message "✍ Writing session started → C-c o = focus • C-c s = spelling • M-$ = correct"))

(global-set-key (kbd "C-c w s") #'sjy2/writing-session)


;;; ———————————————————————— 20 Org Shoggoth ———————————————————————

(use-package org
  :ensure nil
  :demand t
  :hook ((org-mode . visual-line-mode)
         (org-mode . variable-pitch-mode)
         (org-mode . org-indent-mode)
         (org-mode . org-modern-mode)          ; pretty everything
         (org-mode . org-appear-mode))         ; show markers only on hover

  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
  :custom
  (org-directory "~/MEGA/emacs-notes/denote/")
  ;; (setopt org-directory    (expand-file-name "org/"        sjy2-etc-dir))
  (org-agenda-files '("~/MEGA/emacs-notes/denote/agenda.org"))
  
  ;; Startup
  (org-startup-indented t)
  (org-startup-folded t)
  (org-startup-with-inline-images t)

  ;; Visual improvements
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-pretty-entities-include-sub-superscripts t)
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-whole-heading-line t)
  (org-ellipsis " ⤵")  ; Note: space before for better spacing
  (org-image-actual-width '(450))
  (org-eldoc-breadcrumb-separator " → ")
  (org-pretty-entities-include-sub-superscripts t)
  
  ;; Editing behaviour
  (org-special-ctrl-a/e t)
  (org-support-shift-select 'always)
  (org-catch-invisible-edits 'smart)
  (org-insert-heading-respect-content t)
  (org-return-follows-link t)  ; RET follows links
  (org-loop-over-headlines-in-active-region t)  ; Operate on region headlines
  
  ;; Source blocks
  (org-src-tab-acts-natively t)
  (org-src-preserve-indentation t)
  (org-edit-src-content-indentation 0)
  (org-src-fontify-natively t)  ; Syntax highlight in blocks
  (org-confirm-babel-evaluate nil)  ; Don't ask before evaluating
  
  ;; TODOs
  (org-log-done 'time)
  (org-log-into-drawer t)  ; Keep logs in :LOGBOOK: drawer
  (org-todo-keywords
   '((sequence "TODO(t)" "HOLD(h)" "WAIT(w)" "INFO(i)" "REDO(r)"
               "|" "DONE(d)" "MISS(m)" "SKIP(s)" "CANC(c)")))
  
  (org-todo-keyword-faces
   '(("TODO" . (:foreground "red"     :weight bold))
     ("HOLD" . (:foreground "orange"  :weight bold))
     ("WAIT" . (:foreground "magenta" :weight bold))
     ("INFO" . (:foreground "green"   :weight bold))
     ("REDO" . (:foreground "red"     :weight bold))
     ("DONE" . (:foreground "green"   :weight bold))
     ("MISS" . (:foreground "gray"    :weight bold))
     ("SKIP" . (:foreground "gray"    :weight bold))
     ("CANC" . (:foreground "gray"    :weight bold))))
  
  ;; Agenda
  (org-agenda-start-with-log-mode t)
  (org-agenda-window-setup 'current-window)

  :config    
  (add-hook 'org-mode-hook
            (lambda ()
              (remove-hook 'post-command-hook 'org--adapt-indentation t))))

;; ———————————————————————— org-modern – Modus-aware ————————————————————————
(use-package org-modern
  :ensure t
  :demand t
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize-hook . org-modern-agenda))
  :custom  
  (org-modern-replace-stars '("◉" "○" "●" "◦" "•" "·"))
  ;; (org-modern-replace-stars '("⭐️" "💎" "☘️" "🐺" "🦊" "🐰"))
  (org-modern-hide-stars 'leading)
  (org-modern-star 'replace)  ; nil or fold (triangles)
  (org-modern-table t)
  (org-modern-todo t)
  (org-modern-priority t)
  (org-modern-checkbox 
   '((?X . "☑")
     (?  . "☐") 
     (?- . "❍")))
  (org-modern-list 
   '((42 . "—")
     (43 . "—") 
     (45 . "—")))
  (org-modern-horizontal-rule "────────────────────")
  (org-modern-radio-target    '("❰" t "❱"))
  (org-modern-internal-target '("↪ " t ""))
  (org-modern-keyword      "‣")
  (org-modern-priority     t)
  (org-modern-tag          t)
  (org-modern-timestamp    t)
  (org-modern-statistics   t)
  (org-modern-block-fringe nil)
  (org-modern-block-name
   '(("src"      . ("⎡" . "⎣"))
     ("example" . ("⎡" . "⎣"))
     ("quote"   . ("❝" . "❞"))
     (t         . ("·" . "·"))))

  (org-modern-block-name
   `(("src" :background ,(cdr (assoc 'sjy2-bg-1        sjy2-modus-palette)) :extend t)
     (t     :background ,(cdr (assoc 'sjy2-bg-1        sjy2-modus-palette)) :extend t)))

  :config
  ;; Fixed: Unified theme-aware faces (runs early, no duplicates)
  (defun sjy2/org-modern-update-faces ()
    "Apply org-modern faces respecting Modus light/dark."
    (let* ((light (eq (frame-parameter nil 'background-mode) 'light))
           (block-bg (if light
                         (cdr (assoc 'sjy2-bg-1 sjy2-modus-palette))
                       "#282828"))
           (meta-bg (if light "#e8e8e8" "#323232"))
           (meta-fg (if light "#666666" "#999999")))
      (custom-set-faces
       `(org-block                ((t (:inherit fixed-pitch :background ,block-bg :extend t))))
       `(org-block-begin-line     ((t (:inherit org-meta-line :background ,meta-bg :foreground ,meta-fg :height 0.9))))
       `(org-block-end-line       ((t (:inherit org-block-begin-line))))
       `(org-code                 ((t (:inherit (shadow fixed-pitch)))))
       `(org-verbatim             ((t (:inherit (shadow fixed-pitch)))))
       `(org-table                ((t (:inherit fixed-pitch))))
       `(org-modern-label         ((t (:inherit fixed-pitch :height 0.9)))))))

  ;; Fixed: Apply on org-mode entry + theme change (no duplicate hooks)
  (add-hook 'org-mode-hook #'sjy2/org-modern-update-faces)
  (add-hook 'modus-themes-after-load-theme-hook #'sjy2/org-modern-update-faces)

  ;; Initial run
  (sjy2/org-modern-update-faces)

  ;;:config
  (global-org-modern-mode 1)
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))


;; Slightly rounded corners (org-modern-indent adds this automatically if installed)
(use-package org-modern-indent
  :after org-modern
  :vc (:url "https://github.com/jdtsmith/org-modern-indent" :rev :newest)
  :config (add-hook 'org-mode-hook #'org-modern-indent-mode))

;; Show hidden emphasis markers depending on position of point.
(use-package org-appear
  ;; :vc (:url "https://github.com/awth13/org-appear"))
  :after org
  :hook
  (org-mode . org-appear-mode))

;; Left-side outline tree
(use-package org-side-tree
  :after org
  :defer t
  :custom
  (org-side-tree-display-side 'left)
  (org-side-tree-fontify t)
  (org-side-tree-persistent nil)
  (org-side-tree-width 24))

;; Auto-tangle + screenshots
(use-package org-auto-tangle   :ensure t :hook (org-mode . org-auto-tangle-mode))
(use-package org-download
  :ensure t
  :after org
  :custom
  (org-download-image-dir "./img")
  (org-download-heading-lvl nil)
  (org-download-screenshot-method "flameshot gui --path %s")
  :bind (:map org-mode-map ("s-Y" . org-download-screenshot)))

;; Capture templates (clean & useful)
(with-eval-after-load 'org-capture
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline org-default-notes-file "Inbox")
           "* TODO %?\n%U")
          ("n" "Note" entry (file+headline org-default-notes-file "Notes")
           "* %? :note:\n%U")
          ("j" "Journal" entry (file+datetree "~/MEGA/emacs-notes/denote/journal.org")
           "* %<%H:%M> %?\n%i"))))

;; One-key perfect writing session
(defun sjy2/org-writing-session ()
  "Today’s journal + focus + spellcheck."
  (interactive)
  (denote-journal)
  (olivetti-mode 1)
  (jinx-mode 1))
(global-set-key (kbd "C-c w j") #'sjy2/org-writing-session)


;;; ———————————————————————— 25 Windowing ————————————————————————

(use-package popper
  :ensure t
  :demand t
  :bind (("C-`"   . popper-toggle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*TeX Help\\*"
          help-mode
          helpful-mode
          compilation-mode
          occur-mode
          messages-buffer-mode
          calendar-mode)
        
        ;; Control where popups appear
        popper-display-function #'display-buffer-in-side-window
        popper-display-control t)
  :custom
  (popper-mode-line '(:eval (propertize " 🍿 " 'face 'mode-line-emphasis)))
  (popper-window-height 0.33)
  :config
  (popper-mode 1)
  (popper-echo-mode 1)
  
  ;; Make cycling repeatable (Karthink's trick)
  (advice-add 'popper-cycle :after
              (lambda (&rest _)
                (when (eq last-command-event ?`)
                  (set-transient-map
                   (let ((map (make-sparse-keymap)))
                     (define-key map "`" #'popper-cycle)
                     map))))))

;; Help buffers appear on right side
(add-to-list 'display-buffer-alist
             '("\\*\\(Help\\|helpful\\).*\\*"
               (display-buffer-in-side-window)
               (side . right)
               (slot . 0)
               (window-width . 80)))

;;; ———————————————————————— 90 sjy2 custom code – 2025 cleaned  ————————————————————————



;; Prot’s keyboard-quit-dwim – kept (still the gold standard)
(defun prot/keyboard-quit-dwim ()
  "Smart `keyboard-quit': close minibuffer, deactivate region, etc."
  (interactive)
  (cond ((region-active-p) (keyboard-quit))
        ((derived-mode-p 'completion-list-mode) (delete-completion-window))
        ((> (minibuffer-depth) 0) (abort-recursive-edit))
        (t (keyboard-quit))))

;;; ———————————————————————— 100 Startup message ————————————————————————
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %.2f s with %d GCs (%s mode)"
                     (float-time (time-subtract after-init-time before-init-time))
                     gcs-done
                     (if (daemonp) "daemon" "regular"))))

(provide 'init.el)
;;; init.el ends here
