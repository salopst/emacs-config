;;; sjy2-md-agenda.el --- Simple markdown Agenda for sjy2 -*- lexical-binding: t; -*-
;; 2026-02-12 | v 0.7

;; super simple "agenda" styling for markdown


(defvar sjy2/md-agenda-tags
  '(("NHS" . "@NHS")
    ("HMC" . "@HMC")
    ("CS" . "@CS")
    ("phone" . "@phone")
    ("remote" . "@remote")
    ("indeed" . "@indeed"))
  "Predefined tags for markdown agenda entries.")

;; Status keyword faces
(defface sjy2/md-agenda-todo
  '((t (:background "#f7768e" :weight bold :box t :foreground "#1a1b26")))
  "Face for TODO.")

(defface sjy2/md-agenda-curr
  '((t (:background "#ff9e64" :weight bold :box t :foreground "#1a1b26")))
  "Face for CURR (current/in-progress).")

(defface sjy2/md-agenda-done
  '((t (:background "#9ece6a" :weight bold :box t :foreground "#1a1b26")))
  "Face for DONE.")

(defface sjy2/md-agenda-info
  '((t (:background "#73daca" :weight bold :box t :foreground "#1a1b26")))
  "Face for INFO.")

(defface sjy2/md-agenda-pass
  '((t (:background "#565f89" :weight bold :box t :foreground "#c0caf5")))
  "Face for PASS.")

;; Heading level faces
(defcustom sjy2/md-agenda-heading-specs
  '((1 . (:foreground "#282828" :weight bold :height 1.15))
    (2 . (:foreground "#565f89" :weight bold :height 1.10))
    (3 . (:foreground "#4b4b4b" :weight bold :height 1.05))
    (4 . (:foreground "#467e88" :weight bold :height 1.0))
    (5 . (:foreground "#282828" :weight bold :height 1.0)))
  "Alist of heading levels to face specifications.
Each entry is (LEVEL . PLIST) where PLIST contains :foreground, :weight, :height."
  :type '(alist :key-type integer :value-type plist)
  :group 'sjy2-md-agenda)

(defcustom sjy2/md-agenda-statuses
  '("TODO" "CURR" "DONE" "INFO" "PASS")
  "List of status keywords to recognize."
  :type '(repeat string)
  :group 'sjy2-md-agenda)

;; Generate heading faces dynamically
(dotimes (i 6)
  (let* ((level (1+ i))
         (face-name (intern (format "sjy2/md-agenda-h%d" level)))
         (spec (alist-get level sjy2/md-agenda-heading-specs)))
    (custom-declare-face face-name `((t ,spec))
                         (format "Face for level %d headings." level))))

;; Generate font-lock keywords dynamically
(defun sjy2/md-agenda--generate-keywords ()
  "Generate font-lock keywords for all status/level combinations."
  (let (keywords)
    ;; Agenda headings with status keywords
    (dolist (status sjy2/md-agenda-statuses)
      (dotimes (i 3) ; Levels 1-3
        (let* ((level (1+ i))
               (hashes (make-string level ?#))
               (status-face (intern (format "sjy2/md-agenda-%s" (downcase status))))
               (heading-face (intern (format "sjy2/md-agenda-h%d" level))))
          (push `(,(format "^\\(%s\\)[ \t]+\\(%s\\)\\(.*\\)$" hashes status)
                  (1 'markdown-markup-face)
                  (2 ',status-face)
                  (3 ',heading-face))
                keywords))))
    
    ;; Normal headings (levels 1-6)
    (dotimes (i 6)
      (let* ((level (1+ i))
             (hashes (make-string level ?#))
             (heading-face (intern (format "sjy2/md-agenda-h%d" level))))
        (push `(,(format "^\\(%s\\)[ \t]+\\(.*\\)$" hashes)
                (1 'markdown-markup-face)
                (2 ',heading-face prepend))
              keywords)))
    
    (nreverse keywords)))

(defvar sjy2/md-agenda--font-lock-keywords
  (sjy2/md-agenda--generate-keywords)
  "Font-lock keywords for sjy2-md-agenda-mode.")

(defun sjy2/md-agenda--iso-week (date)
  "Return ISO week number (string) for DATE (YYYY-MM-DD)."
  (let* ((time (date-to-time (concat date " 00:00")))
         (week (format-time-string "%V" time)))
    (concat "w" week)))

(defun sjy2/md-agenda--day-abbrev (date)
  "Return three-letter day abbreviation for DATE."
  (format-time-string "%a" (date-to-time (concat date " 00:00"))))

(defun sjy2/md-agenda-new-entry ()
  "Insert a new structured markdown agenda entry."
  (interactive)
  (let* ((status (completing-read "Status: " sjy2/md-agenda-statuses
                                  nil t nil nil "TODO"))
         (date (read-string "Date (YYYY-MM-DD): "
                            (format-time-string "%Y-%m-%d")))
         (week (sjy2/md-agenda--iso-week date))
         (day (sjy2/md-agenda--day-abbrev date))
         (time (read-string "Time (HH:MM or blank): "))
         (tags (completing-read-multiple "Tags (@): "
                                         (mapcar #'car sjy2/md-agenda-tags)
                                         nil t))
         (tag-string (mapconcat (lambda (tag)
                                  (alist-get tag sjy2/md-agenda-tags
                                             nil nil #'string=))
                                tags " "))
         (time-string (if (string-empty-p time) "" (concat " " time))))
    (beginning-of-line)
    (insert (format "# %s %s %s %s%s %s -- "
                    status date week day time-string tag-string))))

(defun sjy2/md-agenda-format-iso-date-at-point ()
  "Replace ISO date at point (YYYY-MM-DD) with date + week + weekday.
Example: 2026-02-19 → 2026-02-19 w08 Thu"
  (interactive)
  (let ((re "\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)"))
    (save-excursion
      (beginning-of-line)
      (if (re-search-forward re (line-end-position) t)
          (let* ((date-str (match-string 0))
                 (year     (string-to-number (match-string 1)))
                 (month    (string-to-number (match-string 2)))
                 (day      (string-to-number (match-string 3)))
                 (time     (encode-time 0 0 12 day month year))
                 (result   (format "%s w%02d %s" date-str
                                   (string-to-number (format-time-string "%V" time))
                                   (format-time-string "%a" time))))
            (replace-match result))
        (message "No ISO date found on this line")))))


(defun sjy2/md-agenda-convert-iso-date (date-str)
  "Insert a formatted date string at point.
Prompt for a date in YYYY-MM-DD format and insert it as: YYYY-MM-DD wWW Day.
Press RET to accept today's date."
  (interactive
   (let ((today (format-time-string "%Y-%m-%d")))
     (list (read-string (format "Date (YYYY-MM-DD) [%s]: " today)
                        nil nil today))))
  (if (string-match "\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)" date-str)
      (let* ((year   (string-to-number (match-string 1 date-str)))
             (month  (string-to-number (match-string 2 date-str)))
             (day    (string-to-number (match-string 3 date-str)))
             (time   (encode-time 0 0 12 day month year))
             (result (format "%s w%02d %s" date-str
                             (string-to-number (format-time-string "%V" time))
                             (format-time-string "%a" time))))
        (insert result))
    (message "Invalid date format, expected YYYY-MM-DD")))



(define-minor-mode sjy2-md-agenda-mode
  "Minor mode for sjy2 markdown agenda highlighting."
  :lighter " sjy2-agenda"
  (if sjy2-md-agenda-mode
      (progn
        (font-lock-add-keywords nil sjy2/md-agenda--font-lock-keywords 'set)
        (font-lock-flush)
        (font-lock-ensure))
    (font-lock-remove-keywords nil sjy2/md-agenda--font-lock-keywords)
    (font-lock-flush)
    (font-lock-ensure)))

(provide 'sjy2-md-agenda)
;;; sjy2-md-agenda.el ends here
