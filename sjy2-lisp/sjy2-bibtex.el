;;; sjy2-bibtex.el --- BibTeX management for sjy2 -*- lexical-binding: t; -*-

(use-package bibtex
  :custom
  (bibtex-autokey-year-length 4)
  (bibtex-align-at-equal-sign t)
  (bibtex-dialect 'BibTeX)
  (bibtex-user-optional-fields
   '(("keywords" "Keywords to describe the entry" "")
     ("file" "Link to a document file" ""))))

(use-package bibtex-completion
  :defer t)

(use-package bibtex-actions
  :vc (:url "https://github.com/mbosley/bibtex-actions" :rev :newest)
  :after bibtex-completion
  :bind (("C-c b a i" . bibtex-actions-insert-citation)
         :map minibuffer-local-map
         ("M-b" . bibtex-actions-insert-preset))
  :custom
  (bibtex-actions-bibliography '("~/Documents/bibtex-library/all-bib-records.bib")))

(use-package biblio
  :defer t
  :bind ("C-c w b b" . ews-bibtex-biblio-lookup))

(use-package citar
  :vc (:url "https://github.com/emacs-citar/citar" :rev :newest)
  :custom
  (org-cite-global-bibliography '("~/Documents/bibtex-library/all-bib-records.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  :bind (("C-c w b o" . citar-open)
         :map org-mode-map
         ("C-c w b i" . org-cite-insert)))

(use-package citar-denote
  :vc (:url "https://github.com/pprevos/citar-denote" :rev :newest)
  :after citar
  :custom
  (citar-open-always-create-notes t)
  (citar-denote-template
   '("template"
     "#+title: %s\n#+date: %s\n#+filetags: %s\n\n")) ;; Add your custom template here
  :bind (("C-c w b c" . citar-create-note)
         ("C-c w b n" . citar-denote-open-note)
         ("C-c w b x" . citar-denote-nocite)
         :map org-mode-map
         ("C-c w b k" . citar-denote-add-citekey)
         ("C-c w b K" . citar-denote-remove-citekey)
         ("C-c w b d" . citar-denote-dwim)
         ("C-c w b e" . citar-denote-open-reference-entry))
  :config
  (citar-denote-mode 1))

(provide 'sjy2-bibtex)
;;; sjy2-bibtex.el ends here
