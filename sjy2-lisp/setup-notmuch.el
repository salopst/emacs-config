;; -*- lexical-binding: t -*-


;; lastmod 2023-12-11T12:01:01

;; to delete mail:
;; notmuch search --output=files --format=text0 tag:del | xargs -r0 rm


(setq mail-user-agent 'message-user-agent)
(setq user-mail-address "stephen@yearl.uk")
(setq user-full-name "Stephen WANK Yearl (UK)")


;; this from $HOME/.notmuch-config
;; (setq user-mail-address (notmuch-user-primary-email))
;; (setq user-full-name (notmuch-user-name))

;; 
;; * NOTMUCH
;;      



     

(use-package notmuch
  :commands notmuch-hello
  :bind (("C-c m" . notmuch-hello))
  :config 
  ;; UI
  (setq notmuch-show-logo t)
  (setq notmuch-column-control 0.5) ;; 0.5 == 2 cols ; 1.0 == single col
  (setq notmuch-hello-auto-refresh t)
  (setq notmuch-hello-recent-searches-max 20)
  (setq notmuch-hello-thousands-separator " ")
  (setq notmuch-hello-sections '(notmuch-hello-insert-saved-searches notmuch-hello-insert-alltags))
  ;; notmuch-mail dir is ~/Maildir, specified in ~/.notmuch-config
  (setq notmuch-hooks-dir (expand-file-name ".notmuch/hooks" notmuch-mail-dir))
  (setq notmuch-show-all-tags-list t)
  ;;(setq notmuch-archive-tags '("-inbox" "-unread" "+archived"))
  ;;(setq notmuch-show-mark-read-tags '("-inbox" "-unread" "+archived"))
  (setq notmuch-search-oldest-first nil)
  (setq notmuch-show-indent-content nil)

  (setq notmuch-search-line-faces
    '(
      ("flagged"   . (:inherit notmuch-search-flagged-face  
                      :foreground "IndianRed"))
	    ("important" . (:foreground "CornflowerBlue"))
      ("unread"    . (:inherit notmuch-search-unread-face))
      ("gov.uk"    . (:foreground "DarkGreen"))
      )))








;; 
;; * SENDMAIL
;;
(require 'smtpmail)

(use-package sendmail
  :after (message notmuch)
  :config
  (setq mail-host-address "protonmail.com"
        send-mail-function 'sendmail-send-it
        message-send-mail-function 'message-send-mail-with-sendmail
        sendmail-program "/usr/bin/msmtp"
        sendmail-program (executable-find "msmtp")
        mail-specify-envelope-from t
        message-sendmail-envelope-from 'header
        message-sendmail-f-is-evil nil
        mail-envelope-from 'header
        mail-interactive t))



(provide 'setup-notmuch)
;; setup-notmuch.el ends here
