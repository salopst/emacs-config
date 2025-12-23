;;; sjy2-devil.el --- Personal Settings -*- lexical-binding: t -*-

;; Devil mode trades your comma key in exchange for a modifier-free editing experience in Emacs. 

;; https://susam.github.io/devil/

(use-package devil
  :config 
  ;; (setq devil-all-keys-repeatable t)
  (setq devil-prompt  " \U0001F608 %t")  ;; get a little devil/imp 👿
  (setq devil-lighter " \U0001F608"))
(global-set-key (kbd "C-,") 'global-devil-mode)

;; ,SPC can be used to set mark. ,,SPC to add ,SPC
;; (assoc-delete-all "%k SPC" devil-special-keys)


(provide 'sjy2-devil)
;;; sjy2-devil.el ends here