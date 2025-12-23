;;; sjy2 Dired Open-With Configuration (v2) -*- lexical-binding: t; -*-

;;; Custom Applications Registry

(defvar sjy2/app-commands
  '(("Sioyek (PDF)"    . "flatpak run com.github.ahrm.sioyek")
    ("TeXstudio"       . "flatpak run org.texstudio.TeXstudio")
    ("Audacity"        . "flatpak run org.audacityteam.Audacity")
    ("VLC"             . "flatpak run org.videolan.VLC")
    ("GIMP"            . "flatpak run org.gimp.GIMP")
    ("Evince"          . "evince")
    ("Zathura (PDF)"   . "zathura")
    ("MPV"             . "mpv")
    ("Firefox"         . "firefox")
    ("LibreOffice"     . "libreoffice")
    ("VSCode"          . "code")
    ("System Default"  . "xdg-open"))
  "Alist of (DISPLAY-NAME . COMMAND) for Dired open-with.
DISPLAY-NAME is shown in completion.
COMMAND is the shell command to execute.")

;;; Main Function

(defun sjy2/dired-open-with ()
  "Open marked files with a chosen application.
Offers:
1. Custom apps from `sjy2/app-commands'
2. AppImages in ~/app-images/"
  (interactive)
  (unless (derived-mode-p 'dired-mode)
    (user-error "Not in Dired"))
  
  (let* ((files (dired-get-marked-files nil current-prefix-arg))
         (file-count (length files))
         (candidates (sjy2/-build-app-candidates))
         (choice (completing-read
                  (format "Open %d file%s with: "
                          file-count
                          (if (> file-count 1) "s" ""))
                  candidates
                  nil nil))
         (command (sjy2/-resolve-command choice))
         (quoted-files (mapconcat #'shell-quote-argument files " ")))
    
    (if command
        (let ((full-command (format "%s %s" command quoted-files)))
          (message "Running: %s" full-command)
          (start-process-shell-command
           "dired-open-with" nil full-command))
      (user-error "Could not resolve command for: %s" choice))))

;;; Helper Functions

(defun sjy2/-build-app-candidates ()
  "Build list of application candidates for completion."
  (delete-dups
   (append
    ;; 1. Custom registered apps (with display names)
    (mapcar #'car sjy2/app-commands)
    
    ;; 2. AppImages in ~/app-images/
    (sjy2/-find-appimages))))

(defun sjy2/-find-appimages ()
  "Find all .AppImage files in ~/app-images/."
  (let ((dir (expand-file-name "~/app-images/")))
    (when (file-directory-p dir)
      (mapcar #'file-name-nondirectory
              (directory-files dir t "\\.AppImage\\'" t)))))

(defun sjy2/-resolve-command (choice)
  "Resolve CHOICE to an executable command.
Checks:
1. Custom app registry
2. Direct command in PATH
3. AppImage full path"
  (or
   ;; 1. Registered app with display name
   (cdr (assoc choice sjy2/app-commands))
   
   ;; 2. Direct executable in PATH
   (executable-find choice)
   
   ;; 3. AppImage (return full path)
   (let ((appimage (expand-file-name choice "~/app-images/")))
     (when (file-exists-p appimage)
       appimage))
   
   ;; 4. Just try the choice as-is (might be a full path)
   choice))

;;; Alternative: File-type Specific Defaults

(defvar sjy2/filetype-defaults
  '(("\\.pdf\\'" . "Sioyek (PDF)")
    ("\\.\\(png\\|jpg\\|jpeg\\|gif\\|webp\\)\\'" . "GIMP")
    ("\\.\\(mp4\\|mkv\\|avi\\|mov\\)\\'" . "MPV")
    ("\\.\\(mp3\\|flac\\|wav\\|ogg\\)\\'" . "MPV")
    ("\\.tex\\'" . "TeXstudio")
    ("\\.\\(odt\\|docx\\|xlsx\\)\\'" . "LibreOffice"))
  "Alist of (REGEX . DEFAULT-APP) for smart defaults.")

(defun sjy2/dired-open-with-smart ()
  "Open files with smart defaults based on file type.
Falls back to `sjy2/dired-open-with' if no match or prefix arg."
  (interactive)
  (if current-prefix-arg
      ;; With prefix: always prompt
      (call-interactively #'sjy2/dired-open-with)
    
    ;; Without prefix: try smart default
    (let* ((files (dired-get-marked-files nil nil))
           (first-file (car files))
           (default-app (sjy2/-guess-app-for-file first-file)))
      
      (if default-app
          (let* ((command (sjy2/-resolve-command default-app))
                 (quoted-files (mapconcat #'shell-quote-argument files " "))
                 (full-command (format "%s %s" command quoted-files)))
            (message "Opening with %s: %s" default-app full-command)
            (start-process-shell-command "dired-open-with" nil full-command))
        
        ;; No default found, prompt
        (call-interactively #'sjy2/dired-open-with)))))

(defun sjy2/-guess-app-for-file (filename)
  "Guess appropriate app for FILENAME based on extension."
  (cdr (seq-find
        (lambda (pair)
          (string-match-p (car pair) filename))
        sjy2/filetype-defaults)))

;;; Keybindings
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C") #'sjy2/dired-create-dir-or-file)
  (define-key dired-mode-map (kbd "J") #'sjy2/dired-goto-dir-or-file)
  ;; O: Always prompt for app
  (define-key dired-mode-map (kbd "O") #'sjy2/dired-open-with)
  ;; o: Smart open (uses defaults, C-u o to prompt)
  (define-key dired-mode-map (kbd "o") #'sjy2/dired-open-with-smart))

;;; Usage Examples

;; Mark PDF files in Dired:
;;   O → Choose app (shows "Sioyek (PDF)", "Evince", etc.)
;;   o → Opens with Sioyek automatically (default for .pdf)
;;   C-u o → Prompt for app (overrides default)

;; Your AppImages automatically appear in completion!

(provide 'sjy2-dired-open-with)
;;; sjy2-dired-open-with.el ends here