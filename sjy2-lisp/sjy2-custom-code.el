;;; sjy2-custom-code.el --- sjy2 custom code -*- lexical-binding: t; -*-
(require 'tex nil t)

;;; ------------------------------------------------------------------
;;; Compile-time Declarations to Silence Native-Comp Warnings
;;; ------------------------------------------------------------------

;; dired
(declare-function dired-get-filename "dired")
(declare-function dired-rename-file "dired")
(declare-function dired-revert "dired")

;; visual-regexp
(declare-function vr/replace "visual-regexp")
(declare-function vr/query-replace "visual-regexp")
(declare-function vr/mark "visual-regexp")

;; deadgrep
(declare-function deadgrep "deadgrep")

;; package-vc (Emacs 30+)
(declare-function package-vc-p "package-vc")
(declare-function package-vc-update "package-vc")

;; your own functions (defined later)
(declare-function sjy2/org-export-pdf-and-open "sjy2/custom-code")
(declare-function sjy2/clean-and-recompile "sjy2/custom-code")
(declare-function sjy2/recompile-init "sjy2/custom-code")
(declare-function sjy2/update-vc-packages "sjy2/custom-code")
(declare-function sjy2/m-m-map "sjy2/custom-code")

;;; ------------------------------------------------------------------
;;; Keymaps
;;; ------------------------------------------------------------------

(defvar sjy2/prefix-map (make-sparse-keymap)
  "Personal keymap under M-m prefix.")
(define-key global-map (kbd "M-m") sjy2/prefix-map)

(defvar sjy2/search-map (make-sparse-keymap)
  "Search-related commands under M-m s.")
(define-key sjy2/prefix-map (kbd "s") sjy2/search-map)

(defmacro sjy2/bind (key command &optional map)
  "Bind KEY to COMMAND in MAP (defaults to `sjy2/prefix-map')."
  `(define-key ,(or map 'sjy2/prefix-map) (kbd ,key) ,command))

;;; ------------------------------------------------------------------
;;; Keybindings
;;; ------------------------------------------------------------------

(keymap-global-set "C-x p d"  #'sjy2/insert-timestamp)
(keymap-global-set "C-x p s"  #'sjy2/rgrep-selected)
(keymap-global-set "C-c e c"  #'sjy2/eval-and-copy)
(keymap-global-set "C-c t t"  #'sjy2/toggle-transparency)
(keymap-global-set "C-c M-t"  #'sjy2/markdown-table-to-org)

;; windowing
(keymap-global-set "C-x 4"    #'sjy2/toggle-maximize-window)
(keymap-global-set "C-x 5"    #'sjy2/window-split-toggle)
(keymap-global-set "C-x 6"    #'sjy2/toggle-window-swap)
(keymap-global-set "C-x o"    #'sjy2/cycle-windows-and-frames)

(keymap-global-set "C->"   #'sjy2/enlarge-window-horizontally)
(keymap-global-set "C-<"   #'sjy2/shrink-window-horizontally)
(keymap-global-set "C-^"   #'sjy2/enlarge-window-vertically)
(keymap-global-set "C-%"   #'sjy2/shrink-window-vertically)

;; winner mode?
;; Winner-mode for C-c <left>/<right> undo/redo of window layouts
(winner-mode 1)
(keymap-global-set "C-s-+"           #'enlarge-window-horizontally)
(keymap-global-set "C-s--"           #'shrink-window-horizontally)

(keymap-global-set "M-s-<return>"    #'toggle-frame-fullscreen)
(keymap-global-set "M-u"             #'sjy2/cycle-case-region-or-word)
(keymap-global-set "C-g"             #'prot/keyboard-quit-dwim)


;; Buffer and file ops
(sjy2/bind "b i" #'back-to-indentation)
(sjy2/bind "b n" #'sjy2/copy-buffer-name)
(sjy2/bind "f d" #'find-name-dired)
(sjy2/bind "f w" #'sjy2/write-file-with-filename)
(sjy2/bind "n n" #'sjy2/copy-file-name)
(sjy2/bind "s d" #'sjy2/save-with-timestamp-prefix)

;; put in direc section of init.el.
(with-eval-after-load 'dired
  (keymap-set dired-mode-map "r" #'sjy2/dired-rename-current-file))

;; Org
(sjy2/bind "o p" #'sjy2/org-export-pdf-and-open)

;; Emacs maintenance
(sjy2/bind "e r" #'sjy2/recompile-init)
(sjy2/bind "e c" #'sjy2/clean-and-recompile)
(sjy2/bind "e u" #'sjy2/update-vc-packages)

;; Text wrapping
(sjy2/bind "("  #'sjy2/wrap-parens)
(sjy2/bind "["  #'sjy2/wrap-brackets)
(sjy2/bind "{"  #'sjy2/wrap-braces)
(sjy2/bind "\"" #'sjy2/wrap-quotes)
(sjy2/bind "/"  #'sjy2/wrap-slashes)
(sjy2/bind "*"  #'sjy2/wrap-asterisks)
(sjy2/bind "~"  #'sjy2/wrap-tildes)
(sjy2/bind "w"  #'sjy2/wrap-interactive)
(sjy2/bind "u"  #'sjy2/unwrap)


;; Development
(sjy2/bind "x e" #'sjy2/eval-and-copy)

;; Search
(sjy2/bind "g" #'deadgrep   sjy2/search-map)
(sjy2/bind "r" #'vr/replace sjy2/search-map)
(sjy2/bind "m" #'vr/mark    sjy2/search-map)
(sjy2/bind "q" #'vr/query-replace sjy2/search-map)

;;; ------------------------------------------------------------------
;;; Implementations
;;; ------------------------------------------------------------------

(defun sjy2-kill-orphan-buffers ()
  "Kill file-visiting buffers whose files no longer exist."
  (interactive)
  (let ((killed 0))
    (dolist (buf (buffer-list))
      (when-let ((file (buffer-file-name buf)))
        (unless (or (file-remote-p file)
                    (file-exists-p file))
          (when (kill-buffer buf)
            (cl-incf killed)))))
    (message "Killed %d orphan buffer%s"
             killed (if (= killed 1) "" "s"))))

(defun sjy2-clean-session ()
  "Kill orphan buffers and clean recentf/desktop.
Skips remote files (TRAMP buffers)."
  (interactive)
  (let ((killed 0)
        (recentf-cleaned 0)
        (desktop-cleaned nil))
    
    ;; 1. Kill buffers with missing files (skip remote)
    (dolist (buf (buffer-list))
      (when-let ((file (buffer-file-name buf)))
        (unless (or (file-remote-p file)
                    (file-exists-p file))
          (when (kill-buffer buf)
            (cl-incf killed)))))
    
    ;; 2. Clean recentf (skip remote)
    (when (bound-and-true-p recentf-mode)
      (let ((original-count (length recentf-list)))
        (setq recentf-list
              (seq-filter (lambda (f)
                            (or (file-remote-p f)
                                (file-exists-p f)))
                          recentf-list))
        (setq recentf-cleaned (- original-count (length recentf-list)))
        (when (> recentf-cleaned 0)
          (recentf-save-list))))
    
    ;; 3. Save desktop (next load will be clean)
    (when (bound-and-true-p desktop-save-mode)
      (desktop-save-in-desktop-dir)
      (setq desktop-cleaned t))
    
    (message "Cleaned: %d buffer%s, %d recentf%s%s"
             killed (if (= killed 1) "" "s")
             recentf-cleaned (if (= recentf-cleaned 1) "" "s")
             (if desktop-cleaned ", saved desktop" ""))))


(defun sjy2/cycle-windows-and-frames ()
  "Sjy2 Cycle through open windows and frames."
  (interactive)
  (let ((windows (window-list))
	      (frames (frame-list)))
    ;; If there are multiple windows, cycle through them
    (if (> (length windows) 1)
	      (select-window (if (eq (selected-window) (car windows))
                           (cadr windows)
			                   (car windows)))
      ;; If there is only one window, cycle through frames
      (let ((current-frame (selected-frame))
            next-frame)
	      (while (and frames
                    (eq (car frames) current-frame))
          (setopt frames (cdr frames)))
	      (if frames
            (setopt next-frame (car frames))
          ;; If there is only one frame, stay on the current frame
          (setopt next-frame current-frame))
	      (select-frame-set-input-focus next-frame)))))

(defun sjy2/toggle-maximize-window ()
  "Temporarily make a window full-screen.
Works with both regular windows and popper popups."
  (interactive)
  (cond
   ;; Case 1: Already maximized, restore
   ((= 1 (length (window-list)))
    (jump-to-register '_))
   
   ;; Case 2: In a popper popup, promote it first then maximize
   ((and (bound-and-true-p popper-popup-status)
         (eq popper-popup-status 'popup))
    (popper-toggle-type)  ; Promote popup to regular window
    (window-configuration-to-register '_)
    (delete-other-windows))
   
   ;; Case 3: Regular window, just maximize
   (t
    (window-configuration-to-register '_)
    (delete-other-windows))))

(defun sjy2/wank ()
  "Rude 'foo'"
  (interactive)
  (message "WANKING WANKER!!!!!"))

;; Core wrapping function
(defun sjy2/wrap-with (left &optional right)
  "Wrap region or symbol with LEFT and RIGHT."
  (let* ((right (or right left))
         (bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (bounds-of-thing-at-point 'symbol))))
    (if bounds
        (save-excursion
          (goto-char (cdr bounds))
          (insert right)
          (goto-char (car bounds))
          (insert left))
      (user-error "No region or symbol"))))

;; Wrapper commands
(defun sjy2/wrap-parens () (interactive) (sjy2/wrap-with "(" ")"))
(defun sjy2/wrap-brackets () (interactive) (sjy2/wrap-with "[" "]"))
(defun sjy2/wrap-braces () (interactive) (sjy2/wrap-with "{" "}"))
(defun sjy2/wrap-quotes () (interactive) (sjy2/wrap-with "\""))
(defun sjy2/wrap-slashes () (interactive) (sjy2/wrap-with "/"))
(defun sjy2/wrap-asterisks () (interactive) (sjy2/wrap-with "*"))
(defun sjy2/wrap-tildes () (interactive) (sjy2/wrap-with "~"))

(defun sjy2/wrap-interactive ()
  (interactive)
  (let* ((left (read-string "Left: "))
         (right (read-string "Right: " nil nil left)))
    (sjy2/wrap-with left right)))


(defun sjy2/unwrap ()
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'sexp)))
    (if bounds
        (save-excursion
          (goto-char (1- (cdr bounds)))
          (delete-char 1)
          (goto-char (car bounds))
          (delete-char 1))
      (user-error "Nothing to unwrap"))))



(defun sjy2/spongebob-case (str)
  "Convert STR to alternating case (Spongebob/mocking case)."
  (let ((result "")
        (upper t))
    (dotimes (i (length str) result)
      (let ((char (aref str i)))
        (setq result (concat result
                             (string (if upper
                                         (upcase char)
                                       (downcase char)))))
        ;; Only toggle for letters (not spaces/punctuation)
        (when (and (>= char ?a) (<= char ?z))
          (setq upper (not upper)))))))


(defun sjy2/cycle-case-region-or-word ()
  "Cycle the case of the active region or the word at point.

Cycle order:
  lower → UPPER → Title Case → Sentence case → sPoNgE cAsE → lower.

If a region is active, operate on the region.  
Otherwise operate on the word at point.

The cycling state persists only if this command is repeated
successively; any other intervening command resets the cycle."
  (interactive)
  (let* ((deactivate-mark nil)
         ;; Determine bounds (region or word at point)
         (bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (bounds-of-thing-at-point 'word))))
    (when bounds
      (let* ((beg (car bounds))
             (end (cdr bounds))
             (text (buffer-substring-no-properties beg end)))

        ;; Reset state if last command wasn't this one.
        (unless (eq last-command this-command)
          (put this-command 'state 0))

        ;; Sentence case: first letter upcased, rest downcased.
        (defun sjy2--sentence-case (s)
          (if (string-empty-p s) s
            (concat (upcase (substring s 0 1))
                    (downcase (substring s 1)))))

        ;; SpongeBob case: alternate upper/lower ignoring spaces.
        (defun sjy2--sponge-case (s)
          (let ((i 0)
                (out ()))
            (dolist (ch (string-to-list s) (apply #'string (nreverse out)))
              (if (char-equal ch ?\s)
                  (push ch out)
                (push (if (= (mod i 2) 0)
                          (upcase ch)
                        (downcase ch))
                      out)
                (setq i (1+ i))))))

        (let* ((variants
                (list
                 (downcase text)
                 (upcase text)
                 (string-titlecase text)
                 (sjy2--sentence-case text)
                 (sjy2--sponge-case text)))
               (state (get this-command 'state))
               (next (nth state variants)))

          ;; Replace region/word
          (delete-region beg end)
          (insert next)

          ;; Keep region active if user started with one
          (when (use-region-p)
            (set-mark (+ beg (length next))))

          ;; Advance cycle state
          (put this-command 'state
               (mod (1+ state) (length variants))))))))
          
(defun sjy2/markdown-table-to-org ()
  "Convert the Markdown table at point to Org table."
  (interactive)
  (let ((table (buffer-substring-no-properties
                (region-beginning) (region-end))))
    (delete-region (region-beginning) (region-end))
    (insert
     (replace-regexp-in-string
      "^|-" "|-"
      (replace-regexp-in-string
       "|-" "+-"
       (replace-regexp-in-string
        "^|" "|"
        table nil t) nil t) nil t))
    (org-table-align)))


(defun sjy2/copy-buffer-name ()
  "Copy current buffer name to clipboard and kill ring."
  (interactive)
  (if-let ((name (buffer-name)))
      (progn
        (kill-new name)
        (message "Copied buffer name: %s" name))
    (user-error "No buffer name available")))

(defun sjy2/copy-file-name ()
  "Copy current file path to clipboard and kill ring.
In Dired, copy the directory path instead."
  (interactive)
  (let ((path (if (derived-mode-p 'dired-mode)
                  default-directory
                (buffer-file-name))))
    (if path
        (progn
          (kill-new path)
          (message "Copied: %s" path))
      (user-error "No file associated with buffer"))))

;;; ----------------------- above are checked ----------------------------

(defun sjy2/save-with-timestamp-prefix ()
  "Save buffer with ISO 8601 timestamp prefix.
Format: YYYYMMDDTHHmm--original-name.ext
Uses file's modification time if it exists, otherwise current time.
Prompts for filename if buffer has no associated file."
  (interactive)
  (let* ((file (or (buffer-file-name)
                   (read-file-name "Save as: ")))
         (dir (file-name-directory file))
         (name (file-name-nondirectory file))
         (mtime (if (file-exists-p file)
                    (file-attribute-modification-time (file-attributes file))
                  (current-time)))
         (timestamp (format-time-string "%Y%m%dT%H%M" mtime))
         (new-file (expand-file-name (format "%s--%s" timestamp name) dir)))
    (write-file new-file)
    (message "Saved as: %s" (file-name-nondirectory new-file))))


(defun sjy2/dired-rename-current-file ()
  "Rename the file at point in Dired with the old name pre-filled.
Provides completion, stays in the same directory, and refreshes Dired cleanly."
  (interactive)
  (unless (derived-mode-p 'dired-mode)
    (user-error "Not in Dired mode"))

  (let* ((file (dired-get-filename nil t))   ; tolerate symlinks & weird lines
         (dir  (file-name-directory file))
         (old  (file-name-nondirectory file))
         ;; minibuffer starts at DIR, prefilled with OLD, and does filename completion
         (new  (read-file-name "Rename to: " dir old nil old)))

    (unless (and new (string= old (file-name-nondirectory new)))
      (dired-rename-file file new nil)
      ;; modern safe dired refresh
      (revert-buffer :ignore-auto :noconfirm)
      (message "Renamed: %s → %s"
               old (file-name-nondirectory new)))))


(defun sjy2/kill-autoloads-buffers ()
  "Kill all buffers visiting files ending with -autoloads.el."
  (interactive)
  (let ((count 0))
    (dolist (buf (buffer-list))
      (when-let* ((name (buffer-name buf))
                  ((string-suffix-p "-autoloads.el" name)))
        (when (kill-buffer buf)
          (setq count (1+ count)))))
    (message "Killed %d autoloads buffer(s)" count)))


(defun sjy2/write-file-with-filename ()
  "Like `write-file`, but pre-fill minibuffer with the current filename."
  (interactive)
  (let* ((cur (or (buffer-file-name) default-directory))
         (dir (file-name-directory cur))
         (default (file-name-nondirectory cur))
         (target (read-file-name "Write file: " dir nil nil default)))
    (write-file target)))


(defun sjy2/insert-timestamp (&optional arg)
  "Insert a timestamp at point.
With prefix ARG (C-u), use ISO8601 format (YYYY-MM-DD HH:MM:SS).
Without ARG, insert compact form: (YYYYMMDD-HHMMSS)."
  (interactive "P")
  (let ((fmt (if arg "%Y-%m-%d %H:%M:%S" "(%Y%m%d-%H%M%S)")))
    (insert (format-time-string fmt (current-time)))))


(defun sjy2/rgrep-selected (beg end)
  "Run `rgrep` on the selected region.
Searches for the literal string in BEG..END within `default-directory`."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (user-error "No region selected")))
  (let* ((str (string-trim (buffer-substring-no-properties beg end))))
    (unless (string-empty-p str)
      (rgrep (shell-quote-argument str) "*" default-directory))))


(defun sjy2/eval-and-copy ()
  "Evaluate the preceding sexp, copy the result to the kill ring, and echo it."
  (interactive)
  (let* ((sexp (preceding-sexp))
         (result (eval sexp t))
         (printed (prin1-to-string result)))
    (kill-new printed)
    (message "Copied: %s" printed)))


(defun sjy2/toggle-transparency ()
  "Toggle frame transparency between 100% and 85%/50%."
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter nil 'alpha
                         (if (or (null alpha) (equal alpha '(100 . 100)))
                             '(85 . 50)
                           '(100 . 100)))))


(defun sjy2/org-export-pdf-and-open ()
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in Org mode"))
  (let ((org-latex-pdf-process
         '("pdflatex -interaction nonstopmode -output-directory %o %f"
           "pdflatex -interaction nonstopmode -output-directory %o %f")))
    (org-latex-export-to-pdf)
    (let ((pdf (concat (file-name-sans-extension (buffer-file-name)) ".pdf")))
      (when (file-exists-p pdf)
        (org-open-file pdf)))))

;;; ------------------------------------------------------------------
;;; Emacs management
;;; ------------------------------------------------------------------

(defcustom sjy2/clean-ignore-dirs
  '("elpa" "straight" ".cache" "eln-cache" ".local" ".git")
  "Directory name substrings to ignore when cleaning .elc files.
Matching is performed on the full path; a match means the file is skipped."
  :type '(repeat string)
  :group 'sjy2)

(defun sjy2/-path-ignored-p (path)
  "Return non-nil if PATH contains any element from `sjy2/clean-ignore-dirs`."
  (seq-some (lambda (ign)
              (string-match-p (regexp-quote (concat "/" ign "/")) path))
            sjy2/clean-ignore-dirs))

(defun sjy2/collect-elc-files (&optional root)
  "Return a list of .elc files under ROOT (defaults to `user-emacs-directory`),
excluding paths that match `sjy2/clean-ignore-dirs`."
  (let ((root (or root user-emacs-directory))
        (files nil))
    (dolist (f (directory-files-recursively root "\\.elc\\'"))
      (unless (sjy2/-path-ignored-p f)
        (push f files)))
    (nreverse files)))

(defun sjy2/clean-elc-files (&optional no-prompt)
  "Delete .elc files under `user-emacs-directory` except ignored dirs.
With a prefix argument (or when NO-PROMPT is non-nil) do not ask for confirmation.
Returns the number of deleted files.

This function does not touch native-comp .eln files by default.
Use `sjy2/clean-eln-files` to remove those intentionally."
  (interactive "P")
  (let* ((files (sjy2/collect-elc-files))
         (n (length files)))
    (unless (or no-prompt
                (yes-or-no-p (format "Delete %d .elc files under %s? " n user-emacs-directory)))
      (user-error "Cancelled"))
    (let ((count 0)
          (reporter (make-progress-reporter "Deleting .elc files..." 0 n)))
      (dolist (f files)
        (condition-case err
            (when (file-exists-p f)
              (delete-file f)
              (setq count (1+ count)))
          (error (message "Failed to delete %s: %s" f (error-message-string err))))
        (progress-reporter-update reporter count))
      (progress-reporter-done reporter)
      (message "Deleted %d .elc file(s)" count)
      count)))

(defun sjy2/recompile-init (&optional force)
  "Byte-recompile Emacs lisp files under `user-emacs-directory`.
If FORCE (interactive prefix) is non-nil, recompile regardless of timestamps.
Opens the *Compile-Log* buffer when finished."
  (interactive "P")
  (let ((default-directory user-emacs-directory))
    ;; 0 -> recompile everything; consider 'force' could set the second arg to t but
    ;; the function signature is (byte-recompile-directory DIR &optional FLAG)
    (byte-recompile-directory user-emacs-directory (if force 0 0))
    (pop-to-buffer "*Compile-Log*")
    (message "Recompilation finished.")))

(defun sjy2/clean-and-recompile (&optional force)
  "Delete non-ignored .elc files then recompile the tree.
With a prefix argument FORCE, pass a compile-forced behavior (interactive).
This is a convenience wrapper that confirms before deletion."
  (interactive "P")
  (when (> (length (sjy2/collect-elc-files)) 0)
    (sjy2/clean-elc-files nil))
  (sjy2/recompile-init force))


;;; === optional: native-comp (eln) cleanup ===

(defun sjy2/collect-eln-files (&optional root)
  "Collect .eln files under ROOT. Defaults to `user-emacs-directory`."
  (let ((files nil)
        (root (or root user-emacs-directory)))
    (dolist (f (directory-files-recursively root "\\.eln\\'"))
      (unless (sjy2/-path-ignored-p f)
        (push f files)))
    (nreverse files)))

(defun sjy2/clean-eln-files (&optional no-prompt)
  "Delete native-comp .eln files under `user-emacs-directory`.
This is destructive; confirm unless NO-PROMPT is non-nil."
  (interactive "P")
  (let ((files (sjy2/collect-eln-files))
        (count 0))
    (unless (or no-prompt
                (yes-or-no-p (format "Delete %d .eln files? " (length files))))
      (user-error "Cancelled"))
    (dolist (f files)
      (condition-case _e
          (when (file-exists-p f)
            (delete-file f)
            (setq count (1+ count)))
        (error (message "Failed to delete %s" f))))
    (message "Deleted %d .eln file(s)" count)
    count))

;;; === update VC-installed packages ===

(defun sjy2/update-vc-packages ()
  "Update all packages installed via `package-vc'.
Silently continues on error, and reports a summary at the end."
  (interactive)
  (unless (bound-and-true-p package--initialized) ; ensure package system available
    (package-initialize))
  (let ((pkgs (mapcar #'car package-alist))
        (count 0)
        (errors '()))
    (dolist (p pkgs)
      (condition-case err
          (when (and (fboundp 'package-vc-p) (package-vc-p p))
            (message "Updating package %s ..." p)
            (package-vc-update p)
            (setq count (1+ count)))
        (error (push (format "%s: %s" p (error-message-string err)) errors))))
    (if errors
        (message "Updated %d package(s) via package-vc; %d errors (see *Messages*)" count (length errors))
      (message "Updated %d package(s) via package-vc" count))))


;;; ------------------------------------------------------------------
;;; Window Resize Advice
;;; ------------------------------------------------------------------

(defun sjy2/enlarge-window-horizontally (&optional repeat)
  "Enlarge window horizontally by 8% of the frame width."
  (interactive "p")
  (enlarge-window-horizontally (* (or repeat 1)
                                  (floor (frame-width) 20))))

(defun sjy2/enlarge-window-vertically (&optional repeat)
  "Enlarge window horizontally by 8% of the frame height."
  (interactive "p")
  (enlarge-window (* (or repeat 1)
                     (floor (frame-height) 20))))

(defun sjy2/shrink-window-horizontally (&optional repeat)
  "Enlarge window horizontally by 8% of the frame width."
  (interactive "p")
  (shrink-window-horizontally (* (or repeat 1)
                                 (floor (frame-width) 20))))
(defun sjy2/shrink-window-vertically (&optional repeat)
  "Enlarge window horizontally by 8% of the frame height."
  (interactive "p")
  (shrink-window (* (or repeat 1)
                    (floor (frame-height) 20))))


;; https://www.reddit.com/r/emacs/comments/mkct2m/can_not_make_any_specific_framepane_to_full/hemfgte/


(defun sjy2/window-split-toggle ()
  "Toggle between horizontal and vertical split with two windows."
  (interactive)
  (if (> (length (window-list)) 2)
      (error "Can't toggle with more than 2 windows!")
    (let ((func (if (window-full-height-p)
		    #'split-window-vertically
                  #'split-window-horizontally)))
      (delete-other-windows)
      (funcall func)
      (save-selected-window
        (other-window 1)
        (switch-to-buffer (other-buffer))))))


(defun sjy2/toggle-window-swap ()
  "Toggle swap between two Emacs windows top/bottom, left/right, regardless of which window is active."
  (interactive)
  (if (= (length (window-list)) 2)
      (let* ((current-window (selected-window))
             (other-window (next-window))
             (edges1 (window-edges current-window))
             (edges2 (window-edges other-window)))
        (if (= (cadr edges1) (cadr edges2))    ; Side-by-side
            (if (< (car edges1) (car edges2))
                (windmove-swap-states-right)   ; Point in left → swap right
              (windmove-swap-states-left))     ; Point in right → swap left
          (if (< (cadr edges1) (cadr edges2))
              (windmove-swap-states-down)      ; Point in top → swap down
            (windmove-swap-states-up))))       ; Point in bottom → swap up
    (message "Only works when exactly two windows are open."))
  (sp-down-sexp)
  (exchange-point-and-mark)
  (sp-backward-down-sexp)
  (exchange-point-and-mark))

;;; ------------------------------------------------------------------

(provide 'sjy2-custom-code)
;;; sjy2-custom-code.el ends here
