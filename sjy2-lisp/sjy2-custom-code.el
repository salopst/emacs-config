;;; sjy2-custom-code.el --- sjy2 custom code -*- lexical-binding: t; byte-compile-warnings: (not free-vars); -*-

;;; Commentary:
;; my bullshit additions.
;; TODO: Simplify, eliminate.


;;; Code:

(require 'tex nil t)
(require 'popper)


;; Compile-time declarations to pacify native-comp and flycheck
(declare-function dired-get-filename "dired")
(declare-function dired-rename-file "dired")
(declare-function dired-revert "dired")
(declare-function vr/replace "visual-regexp")
(declare-function vr/query-replace "visual-regexp")
(declare-function vr/mark "visual-regexp")
(declare-function deadgrep "deadgrep")
(declare-function package-vc-p "package-vc")
(declare-function package-vc-update "package-vc")

;;; ———————————————————————— Keymaps ————————————————————————

(defvar sjy2/prefix-map (make-sparse-keymap)
  "Personal keymap under `M-m' prefix.")
(define-key global-map (kbd "M-m") sjy2/prefix-map)

(defvar sjy2/search-map (make-sparse-keymap)
  "Search-related commands under `M-m s'.")
(define-key sjy2/prefix-map (kbd "s") sjy2/search-map)

(defmacro sjy2/bind (key command &optional map)
  "Bind KEY to COMMAND in MAP (defaults to `sjy2/prefix-map')."
  `(define-key ,(or map 'sjy2/prefix-map) (kbd ,key) ,command))


;;; ———————————————————————— Keybindings ————————————————————————
(keymap-global-set "C-x p s"  #'sjy2/rgrep-selected)
(keymap-global-set "C-c e c"  #'sjy2/eval-and-copy)  ; also M-m c e
(keymap-global-set "C-c t t"  #'sjy2/toggle-transparency)
(keymap-global-set "C-c w c"  #'sjy2/whitespace-clean-in-region)
(keymap-global-set "M-u"      #'sjy2/cycle-case-region-or-word)
(keymap-global-set "M-w"      #'sjy2/kill-ring-save-dwim)
(keymap-global-set "C-x C-w" #'sjy2/write-file)

;; windowing
(keymap-global-set "M-s-<return>"    #'toggle-frame-fullscreen)
(keymap-global-set "C-x 4"  #'sjy2/toggle-maximize-window)
(keymap-global-set "C-x 5"  #'sjy2/window-split-toggle)
(keymap-global-set "C-x 6"  #'sjy2/toggle-window-swap)
(keymap-global-set "C-x o"  #'sjy2/cycle-windows-and-frames)

(keymap-global-set "C-s-="  #'sjy2/enlarge-window-horizontally)
(keymap-global-set "C-s--"  #'sjy2/shrink-window-horizontally)
(keymap-global-set "C-s-]"  #'sjy2/enlarge-window-vertically)
(keymap-global-set "C-s-["  #'sjy2/shrink-window-vertically)

(keymap-global-set "C->"    #'sjy2/enlarge-window-horizontally)
(keymap-global-set "C-<"    #'sjy2/shrink-window-horizontally)
(keymap-global-set "C-^"    #'sjy2/enlarge-window-vertically)
(keymap-global-set "C-%"    #'sjy2/shrink-window-vertically)



;; Buffer and file ops
(sjy2/bind "c"   #'sjy2/copy-current-buffer-name--or-file-path)
(sjy2/bind "s d" #'sjy2/save-with-timestamp-prefix)
(sjy2/bind "o p" #'sjy2/org-export-pdf-and-open)

;; Emacs maintenance
(sjy2/bind "e r" #'sjy2/recompile-init)
(sjy2/bind "e c" #'sjy2/clean-and-recompile)
(sjy2/bind "e u" #'sjy2/update-vc-packages-quietly) ;; works

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
(sjy2/bind "g" #'deadgrep         sjy2/search-map)
(sjy2/bind "r" #'vr/replace       sjy2/search-map)
(sjy2/bind "m" #'vr/mark          sjy2/search-map)
(sjy2/bind "q" #'vr/query-replace sjy2/search-map)


;;; ------------------------------------------------------------------
;;; Implementations
;;; ------------------------------------------------------------------

;; M-w = copy line when no region (like VS Code C-c)
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


(defun sjy2/write-file (filename)
  "Like `write-file' but prepopulates prompt with current filename, point at end."
  (interactive
   (minibuffer-with-setup-hook #'end-of-line
     (list (read-file-name "Write file: "
                           default-directory nil nil
                           (or (and (buffer-file-name)
                                    (file-name-nondirectory (buffer-file-name)))
                               (buffer-name))))))
  (write-file filename))


(defun sjy2/whitespace-clean-in-region (beg end)
  "Clean up whitespace in region (BEG END).  Remove leading/trailing, compress multiple spaces."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      ;; Remove leading whitespace from each line
      (while (re-search-forward "^[[:space:]]+" nil t)
        (replace-match ""))
      ;; Remove trailing whitespace from each line
      (goto-char (point-min))
      (while (re-search-forward "[[:space:]]+$" nil t)
        (replace-match ""))
      ;; Compress multiple spaces to single space
      (goto-char (point-min))
      (while (re-search-forward "[[:space:]]\\{2,\\}" nil t)
        (replace-match " ")))))


(defun sjy2/kill-orphan-buffers ()
  "Kill file-visiting buffers whose files no longer exist."
  (interactive)
  (let ((killed 0))
    (dolist (buf (buffer-list))
      (when-let ((file (buffer-file-name buf)))
        (unless (or (file-remote-p file)
                    (file-exists-p file))
          (when (kill-buffer buf)
            (cl-incf killed)))))
    (message (if (zerop killed)
		 "No orphan buffers found"
               (format "Killed %d orphan buffer%s"
                       killed (if (= killed 1) "" "s"))))))


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
            (setq next-frame (car frames))
          ;; If there is only one frame, stay on the current frame
          (setq next-frame current-frame))
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
(defun sjy2/wrap-parens ()
  "Wrap region or word at point in parentheses."
  (interactive) (sjy2/wrap-with "(" ")"))

(defun sjy2/wrap-brackets ()
  "Wrap region or word at point in square brackets."
  (interactive) (sjy2/wrap-with "[" "]"))

(defun sjy2/wrap-braces ()
  "Wrap region or word at point in curly braces."
  (interactive) (sjy2/wrap-with "{" "}"))

(defun sjy2/wrap-quotes ()
  "Wrap region or word at point in double quotes."
  (interactive) (sjy2/wrap-with "\""))

(defun sjy2/wrap-slashes ()
  "Wrap region or word at point in forward slashes."
  (interactive) (sjy2/wrap-with "/"))

(defun sjy2/wrap-asterisks ()
  "Wrap region or word at point in asterisks (Org/Markdown bold)."
  (interactive) (sjy2/wrap-with "*"))

(defun sjy2/wrap-tildes ()
  "Wrap region or word at point in tildes (Org code markup)."
  (interactive) (sjy2/wrap-with "~"))

(defun sjy2/wrap-interactive ()
  "Wrap region or word at point with user-supplied delimiters.
Prompts for left and right strings, defaulting right to left if omitted."
  (interactive)
  (let* ((left  (read-string "Left: "))
         (right (read-string "Right: " nil nil left)))
    (sjy2/wrap-with left right)))

(defun sjy2/unwrap ()
  "Remove the enclosing delimiter pair around the sexp at point."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'sexp)))
    (if bounds
        (save-excursion
          (goto-char (1- (cdr bounds)))
          (delete-char 1)
          (goto-char (car bounds))
          (delete-char 1))
      (user-error "Nothing to unwrap"))))


;; ---- case switching

(defun sjy2--sentence-case (s)
  "Upcase first character of S, downcase the rest."
  (if (string-empty-p s) s
    (concat (upcase (substring s 0 1))
            (downcase (substring s 1)))))

(defun sjy2--cap-first-alpha (s)
  "Upcase the first alphabetic character in S, leaving the rest unchanged."
  (let* ((i (string-match "[[:alpha:]]" s)))
    (if (null i) s
      (concat (substring s 0 i)
              (upcase (substring s i (1+ i)))
              (substring s (1+ i))))))

(defun sjy2--sponge-case (s)
  "Alternate upper/lower case characters in S, skipping spaces."
  (let ((i 0) out)
    (dolist (ch (string-to-list s) (apply #'string (nreverse out)))
      (if (char-equal ch ?\s)
          (push ch out)
        (push (if (zerop (mod i 2)) (upcase ch) (downcase ch)) out)
        (setq i (1+ i))))))

(defun sjy2--title-case (s)
  "Capitalise the first letter of each word in S."
  (mapconcat #'capitalize (split-string s " ") " "))

(defun sjy2/cycle-case-region-or-word ()
  "Cycle case of region or word at point.
Region order: lower → UPPER → Title Case → Sentence case → Cap first → sPoNgE cAsE → lower.
Word order:   lower → UPPER → Title Case → Sentence case → sPoNgE cAsE → lower.
Cycle state resets if any other command intervenes."
  (interactive)
  (let* ((had-region  (use-region-p))
         (bounds      (if had-region
                          (cons (region-beginning) (region-end))
                        (bounds-of-thing-at-point 'word)))
         (deactivate-mark nil))
    (when bounds
      (let* ((beg  (car bounds))
             (end  (cdr bounds))
             (text (buffer-substring-no-properties beg end)))
        (unless (eq last-command this-command)
          (put this-command 'state 0))
        (let* ((variants (if had-region
                             (list (downcase text)
                                   (upcase text)
                                   (sjy2--title-case text)
                                   (sjy2--sentence-case text)
                                   (sjy2--cap-first-alpha text)
                                   (sjy2--sponge-case text))
                           (list (downcase text)
                                 (upcase text)
                                 (sjy2--title-case text)
                                 (sjy2--sentence-case text)
                                 (sjy2--sponge-case text))))
               (state (get this-command 'state))
               (next  (nth state variants)))
          (delete-region beg end)
          (insert next)
          (when had-region
            (set-mark beg)
            (goto-char (+ beg (length next)))
            (activate-mark))
          (put this-command 'state
               (mod (1+ state) (length variants))))))))


(defun sjy2/copy-current-buffer-name--or-file-path ()
  "Copy the current file path or buffer name to the kill ring and system clipboard.
- In a file-visiting buffer, copies the full file path.
- In Dired, copies the directory path.
- Otherwise, copies the buffer name (for scratch, *Messages*, etc)."
  (interactive)
  (let ((text
         (cond
          ((derived-mode-p 'dired-mode)
           default-directory)
          ((buffer-file-name)
           (buffer-file-name))
          (t
           (buffer-name)))))
    (kill-new text)
    ;; Also copy to system clipboard
    (when (fboundp 'gui-set-selection)
      (gui-set-selection 'CLIPBOARD text))
    (message "Copied: %s" text)))


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
  (let* ((file (dired-get-filename nil t))
         (dir  (file-name-directory file))
         (old  (file-name-nondirectory file))
         (new  (read-file-name "Rename to: " dir old nil old)))
    (unless (string= old (file-name-nondirectory new))
      (dired-rename-file file new nil)
      (revert-buffer :ignore-auto :noconfirm)
      (message "Renamed: %s → %s" old (file-name-nondirectory new)))))


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
  "Export FILE to PDF and open."
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

;;; ———————————————————————— Emacs management ————————————————————————

(defcustom sjy2/clean-ignore-dirs
  '("elpa" "straight" ".cache" "eln-cache" ".local" ".git")
  "Directory name substrings to skip when collecting compiled files."
  :type '(repeat string)
  :group 'sjy2)

(defun sjy2/-path-ignored-p (path)
  "Return non-nil if PATH contains any entry from `sjy2/clean-ignore-dirs'."
  (seq-some (lambda (ign)
              (string-match-p (regexp-quote (concat "/" ign "/")) path))
            sjy2/clean-ignore-dirs))

(defun sjy2/-collect-compiled-files (ext &optional root)
  "Return non-ignored files matching EXT under ROOT (default: `user-emacs-directory')."
  (seq-reject #'sjy2/-path-ignored-p
              (directory-files-recursively
               (or root user-emacs-directory)
               (concat "\\." ext "\\'"))))

(defun sjy2/-delete-compiled-files (ext &optional no-prompt)
  "Delete compiled files matching EXT under `user-emacs-directory'.
Prompt unless NO-PROMPT is non-nil.  Returns count of deleted files."
  (let* ((files (sjy2/-collect-compiled-files ext))
         (n     (length files)))
    (unless (or no-prompt
                (yes-or-no-p (format "Delete %d .%s files under %s? "
                                     n ext user-emacs-directory)))
      (user-error "Cancelled"))
    (let ((count    0)
          (reporter (make-progress-reporter (format "Deleting .%s files..." ext) 0 n)))
      (dolist (f files)
        (condition-case err
            (when (file-exists-p f)
              (delete-file f)
              (setq count (1+ count)))
          (error (message "Failed to delete %s: %s" f (error-message-string err))))
        (progress-reporter-update reporter count))
      (progress-reporter-done reporter)
      (message "Deleted %d .%s file(s)" count ext)
      count)))

(defun sjy2/clean-elc-files (&optional no-prompt)
  "Delete .elc files under `user-emacs-directory', skipping `sjy2/clean-ignore-dirs'.  NO-PROMPT."
  (interactive "P")
  (sjy2/-delete-compiled-files "elc" no-prompt))

(defun sjy2/clean-eln-files (&optional no-prompt)
  "Delete native-comp .eln files under `user-emacs-directory'.  Destructive.  NO-PROMPT."
  (interactive "P")
  (sjy2/-delete-compiled-files "eln" no-prompt))

(defun sjy2/recompile-init (&optional force)
  "Byte-recompile `user-emacs-directory'.
FORCE (prefix arg) recompiles all files regardless of timestamps."
  (interactive "P")
  (byte-recompile-directory user-emacs-directory (if force 0 nil))
  (pop-to-buffer "*Compile-Log*"))

(defun sjy2/clean-and-recompile (&optional force)
  "Delete .elc files then recompile.  FORCE prefix recompiles everything."
  (interactive "P")
  (sjy2/clean-elc-files :no-prompt)
  (sjy2/recompile-init force))

;; === update VC-installed packages ===

(defun sjy2/update-vc-packages-quietly ()
  "Upgrade all VC-installed packages without spawning windows.
Results summarised in *VC Package Upgrades*."
  (interactive)
  (unless (bound-and-true-p package--initialized)
    (package-initialize))
  (let ((buf (get-buffer-create "*VC Package Upgrades*"))
        (upgraded '())
        (failed '())
        (up-to-date '()))
    (with-current-buffer buf
      (erase-buffer)
      (insert (format "VC Package Upgrade — %s\n%s\n\n"
                      (format-time-string "%Y-%m-%d %H:%M")
                      (make-string 40 ?─))))
    (dolist (pkg package-alist)
      (let* ((desc (cadr pkg))
             (name (package-desc-name desc))
             (dir  (and (package-vc-p desc) (package-desc-dir desc))))
        (when dir
          (let* ((default-directory dir)
                 (before (string-trim (shell-command-to-string "git rev-parse HEAD")))
                 (_fetch (shell-command-to-string "git fetch --quiet"))
                 (after  (string-trim (shell-command-to-string "git rev-parse @{u} 2>/dev/null"))))
            (if (string= before after)
                (push name up-to-date)
              (let ((result (shell-command-to-string "git pull --quiet")))
                (if (string-match-p "\\(error\\|fatal\\)" result)
                    (push (cons name result) failed)
                  (push name upgraded))))))))
    (with-current-buffer buf
      (insert (format "✔ Upgraded (%d): %s\n\n" (length upgraded)
                      (mapconcat #'symbol-name (reverse upgraded) ", ")))
      (insert (format "— Up to date (%d): %s\n\n" (length up-to-date)
                      (mapconcat #'symbol-name (reverse up-to-date) ", ")))
      (when failed
        (insert (format "✘ Failed (%d):\n" (length failed)))
        (dolist (f (reverse failed))
          (insert (format "  %s: %s\n" (car f) (cdr f)))))
      (goto-char (point-min)))
    (pop-to-buffer buf)
    (message "VC upgrade complete: %d upgraded, %d failed, %d up to date"
             (length upgraded) (length failed) (length up-to-date))))


;;; ———————————————————————— Window resizing ————————————————————————

(defun sjy2/window-step (dim)
  "Return 5% of frame DIM as a resize step (:width or :height)."
  (floor (if (eq dim :width) (frame-width) (frame-height)) 20))

(defun sjy2/enlarge-window-horizontally (&optional n)
  "Enlarge window horizontally by 5% of frame width, or N columns."
  (interactive "p")
  (enlarge-window-horizontally (* (or n 1) (sjy2/window-step :width))))

(defun sjy2/shrink-window-horizontally (&optional n)
  "Shrink window horizontally by 5% of frame width, or N columns."
  (interactive "p")
  (shrink-window-horizontally (* (or n 1) (sjy2/window-step :width))))

(defun sjy2/enlarge-window-vertically (&optional n)
  "Enlarge window vertically by 5% of frame height, or N rows."
  (interactive "p")
  (enlarge-window (* (or n 1) (sjy2/window-step :height))))

(defun sjy2/shrink-window-vertically (&optional n)
  "Shrink window vertically by 5% of frame height, or N rows."
  (interactive "p")
  (shrink-window (* (or n 1) (sjy2/window-step :height))))


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
    (message "Only works when exactly two windows are open.")))


(provide 'sjy2-custom-code)
;;; sjy2-custom-code.el ends here
