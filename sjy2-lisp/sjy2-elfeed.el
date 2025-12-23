;;; sjy2-elfeed.el --- News reading -*- lexical-binding: t; -*-(require 'tex)

(setopt elfeed-db-directory "~/Documents/news-feeds")
(setopt elfeed-enclosure-default-dir (expand-file-name "~/Downloads"))
(keymap-global-set "C-x y" #'elfeed)
;; integrate with media player
;; https://sqrtminusone.xyz/posts/2021-09-07-emms/

;; Elfeed is an extensible web feed reader for Emacs, supporting both Atom and RSS.
;; https://github.com/skeeto/elfeed
(use-package elfeed
    :bind (:map elfeed-search-mode-map
              ("q" . bjm/elfeed-save-db-and-bury)
              ("Q" . bjm/elfeed-save-db-and-bury)
              ("m" . elfeed-toggle-star)
              ("M" . elfeed-toggle-star)
              ("b" . mz/elfeed-browse-url)
              ("B" . elfeed-search-browse-url)
              ("s" . sjy2/elfeed-tag-sort)
              )
  :config
  (defalias 'elfeed-toggle-star
    (elfeed-expose #'elfeed-search-toggle-all 'star)))

;; Configure the Elfeed RSS reader with an Orgmode file.
(use-package elfeed-org
  (:vc (:url " https://github.com/resjy2honig/elfeed-org" :rev :newest)
  :config (elfeed-org)
  (setopt rmh-elfeed-org-files (list "~/MEGAssync/emacs/elfeed.org")))
(elfeed-org)

;; Various bits and pieces to enhance the Elfeed user experience.
;; https://github.com/jeetelongname/elfeed-goodies
(use-package elfeed-goodies
    :config
  (elfeed-goodies/setup))

;; A richer, interactive, noise-free interface to your Youtube subscriptions.
;; https://github.com/karthink/elfeed-tube
(use-package elfeed-tube
  )

(defun sjy2/elfeed-tag-sort (a b)
  (let* ((a-tags (format "%s" (elfeed-entry-tags a)))
         (b-tags (format "%s" (elfeed-entry-tags b))))
    (if (string= a-tags b-tags)
        (< (elfeed-entry-date b) (elfeed-entry-date a)))
    (string< a-tags b-tags)))

(setf elfeed-search-sort-function #'sjy2/elfeed-tag-sort)

(defun sjy2/elfeed-mark-all-as-read ()
  (interactive)
  (mark-whole-buffer)
  (elfeed-search-untag-all-unread))

(defun sjy2/elfeed-search-filter-source (entry)
  "Filter elfeed search buffer by the feed under cursor."
  (interactive (list (elfeed-search-selected :ignore-region)))
  (when (elfeed-entry-p entry)
    (elfeed-search-set-filter
     (concat
      "@6-months-ago "
      "+unread "
      "="
      (replace-regexp-in-string
       (rx "?" (* not-newline) eos)
       ""
       (elfeed-feed-url (elfeed-entry-feed entry)))))))

;;functions to support syncing .elfeed between machines
;;makes sure elfeed reads index from disk before launching
(defun bjm/elfeed-load-db-and-open ()
  "Wrapper to load the elfeed db from disk before opening"
  (interactive)
  (elfeed-db-load)
  (elfeed)
  (elfeed-search-update--force))

;;write to disk when quiting
(defun bjm/elfeed-save-db-and-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer"
  (interactive)
  (elfeed-db-save)
  (quit-window))   

(provide 'sjy2-elfeed)
;;; sjy2-elfeed.el ends here